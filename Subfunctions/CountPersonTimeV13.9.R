
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

CountPersonTime <- function(Dataset_events = NULL, Dataset, Person_id, Start_study_time, End_study_time, Start_date,
                            End_date, Birth_date = NULL,Rec_period = NULL, Strata = NULL,Outcomes_nrec = NULL,
                            Outcomes_rec = NULL, Name_event = NULL, Date_event = NULL, Age_bands = NULL,
                            Unit_of_age = "year" , Increment = "year", include_remaning_ages = T, Aggregate = T,
                            print = F, check_overlap = T, save_intermediate = NULL, load_intermediate = F){
  
  if(print) print("Version 13.9")
  # Check if demanded R packages are installed, install if not,  and activate
  ################################################################################################################################
  if(print) print("Check packages data.table and lubridate")
  if (!require("data.table")) install.packages("data.table")
  library(data.table)
  
  if (!require("lubridate")) install.packages("lubridate")
  library(lubridate)
  ################################################################################################################################
  
  if(!is.null(Dataset_events) & length(c(Outcomes_nrec, Outcomes_rec)) == 0) stop("No events specified in Outcomes_rec or Outcomes_nrec")
  if(is.null(Dataset_events) & length(c(Outcomes_nrec, Outcomes_rec)) > 0) stop("No Dataset_events specified while Outcomes_rec or Outcomes_nrec are specified")
  if(any(Outcomes_rec %in% Outcomes_nrec)){stop("Overlapping event names for Outcomes_rec and Outcomes_nrec")}
  if(length(Outcomes_rec) != length(Rec_period)) stop("Outcomes_rec and Rec_period are not of the same length")
  
  if (!is.logical(load_intermediate)) {stop("Parameter 'load_intermediate' accepts only logical constants")}
  if (!missing(save_intermediate)) {
    tryCatch(dirname(save_intermediate), error = function(e)
      stop("Parameter 'save_intermediate' accepts only valid filepaths with already existing folders"))
  }
  
  if (load_intermediate) {
    if (!missing(save_intermediate) && file.exists(save_intermediate)) {
      load(save_intermediate)
    } else {
      Dataset <- as.data.table(Dataset)
    }
  } else {
    
    #Set character input for study dates to date format
    ################################################################################################################################
    if(print) print("Assign date format to Start_study_time and End_study_time")
    Start_study_time <- as.IDate(as.character(Start_study_time),"%Y%m%d")
    End_study_time <- as.IDate(as.character(End_study_time),"%Y%m%d")
    ################################################################################################################################
    
    CheckAndPrepareDates(Dataset = Dataset, Person_id = Person_id, Start_study = Start_study_time, End_study = End_study_time, Start_date = Start_date, End_date = End_date, 
                          Birth_date = Birth_date, Age_bands = Age_bands, Increment = Increment, print = print, check_overlap = check_overlap
    )
    gc()
    
    #Select relevant data
    ################################################################################################################################
    intv <- as.IDate(c(Start_study_time, End_study_time))
  
    Dataset <- Dataset[, c(Person_id, Start_date, End_date, Birth_date, Strata) , with = F][get(Start_date) %between% intv|get(End_date) %between% intv|(get(Start_date) < Start_study_time & get(End_date) > End_study_time)] 
    
    if(!is.null(Dataset_events)){
    Dataset_events <- Dataset_events[, c(Person_id, Name_event, Date_event) , with = F][
        get(Date_event) %between% intv & 
        get(Name_event) %in% c(Outcomes_nrec, Outcomes_rec) &
        get(Person_id) %in% unique(Dataset[[Person_id]])  
      , ]
    
    
    tmpname <- tempfile(pattern = "events", tmpdir = tempdir(), fileext = ".rds")
    saveRDS(Dataset_events, tmpname)
    
    }
    
    rm(Dataset_events)
    gc()
    
    if(nrow(Dataset) == 0){
      Dataset <- NULL
      if(print) print("No subjects with any observation time within studyperiod. NULL is returned")
      return(Dataset)
    }
    
    Dataset[get(Start_date) < Start_study_time,eval(Start_date) := Start_study_time]
    Dataset[get(End_date) > End_study_time,eval(End_date) := End_study_time]
    
    
    tmpname2 <- tempfile(pattern = "periods", tmpdir = tempdir(), fileext = ".rds")
    saveRDS(Dataset, tmpname2)
  
    #Determine the ages at the beginning and end of all observation periods. Output is a starting point for calculation and splitting of
    # age bands
    ################################################################################################################################
    if(!is.null(Age_bands)){
      
      if(print) print(paste0("Calculate ages at the start and end of every observation period by ", Increment))
      
      if (nrow(Dataset) > 0){
        
        Dataset[, age_start := floor(time_length(interval(get(Birth_date), get(Start_date)), Unit_of_age)) ]
        Dataset[, age_end := floor(time_length(interval(get(Birth_date), get(End_date)), Unit_of_age)) ]
        
      } else{
        Dataset[,age_start := NA]
        Dataset[,age_end := NA]
      }   
      
    }
    ################################################################################################################################
    
    #Calculate agebands in 2 steps ((1)split/recalculate start/end ages and (assign row to ageband) )
    ################################################################################################################################
    if(!is.null(Age_bands)){
      if(print) print("Create agebands")
      if(nrow(Dataset) > 0){
        #### New code from version 13.6
        
        #Produce a dataset with Agebands and the start and end age of that ageband. This can be used to merge top all cases in the Dataset that overlap with the start and end age. 
        Agebands_list <-  CreateAgebandIntervals(Age_bands, include = include_remaning_ages)
      
        #Merge the overlapping
        setkeyv(Dataset, c("age_start","age_end"))
        Dataset <- foverlaps(Agebands_list, Dataset, by.x = c("ST","EN"), nomatch = 0L, type = "any")
        
        # select the rows that doubled by the merge. In these, multiple agebands occur witing the obeservation period. So start and end dated need to be adapted
        Dataset <- Dataset[, row := row.names(Dataset)]
        Dataset <- Dataset[age_start < ST  ,eval(Start_date) := as.IDate(add_with_rollback(get(Birth_date), period(ST,units = Unit_of_age), roll_to_first = T, preserve_hms = T)), by = row]
        Dataset <- Dataset[age_end > EN  ,eval(End_date) := as.IDate(add_with_rollback(get(Birth_date), period(EN + 1,units = Unit_of_age), roll_to_first = T, preserve_hms = T)) - 1, by = row]
        Dataset <- Dataset[,':=' (age_start = NULL, age_end = NULL,ST = NULL, EN = NULL, row = NULL)]
        
        }
    }else{Agebands_list = NULL}
  
  #Enlarge table by time increment. 
  ################################################################################################################################
  
  
  if(print) print(paste0("Transform input date to a dataset per ", Increment, ". This step increases the size of the file with respect to the choosen increment" ))
  
  #Create a file with the relevant time intervals like with age bands.This is used for joining with the aim to assign labels 
  Dummy <- CreateTimeIntervals(Start_study_time = Start_study_time, End_study_time = End_study_time, Increment = Increment)
    
  setkeyv(Dataset, c(Start_date, End_date))
  Dataset <- foverlaps(Dummy, Dataset, by.x=c(Increment, "End"), nomatch = 0L, type = "any")
  
  Dataset <- Dataset[get(Start_date) <= get(Increment) & get(End_date) >= get(Increment),eval(Start_date) := get(Increment)]
  Dataset <-Dataset[get(End_date) >= End & get(Start_date) <= End, eval(End_date) := End][, End := NULL]
  
  #rm(Dummy)
  gc()
  
  ################################################################################################################################
  
  #if(!is.null(Age_bands)){Age_band_coln <- "Ageband"} else Age_band_coln<-"Ageband" <- NULL
  
  if(print) print("Calculate general persontime")
  
  Dataset[,Persontime := .(get(End_date) - get(Start_date) + 1)]
  if(Increment=="month"){Dataset[,eval(Increment) := substr(get(Increment),1,7)]}
  if(Increment=="year"){Dataset[,eval(Increment) := substr(get(Increment),1,4)]}
  
  if (!missing(save_intermediate)) {
    save(Dataset, file = save_intermediate)
  } 
  
  #add parameters for rest of script
  ###
  if(is.null(Age_bands)){
    
    by_colls <- c(Strata, Increment)
    sort_order <- c(Person_id, Start_date, End_date, Strata)}else{
    
    by_colls <- c(Strata, Increment, "Ageband")  
    sort_order <- c(Person_id, Start_date, End_date, "Ageband", Strata)
  
    }
  
  if(Aggregate) sort_order <- by_colls
  
  if(length(c(Outcomes_nrec, Outcomes_rec)) > 0) coln <- c(sort_order, Increment, "Persontime", 
                                                           paste0("Persontime_",c(Outcomes_nrec, Outcomes_rec)),
                                                           paste0(c(Outcomes_nrec, Outcomes_rec),"_b")
                                                           )else{coln <- c(sort_order, Increment, "Persontime")}
  
  
  
  if(!exists("tmpname") & Aggregate){
    
    Dataset <- Dataset[, lapply(.SD, sum), .SDcols = "Persontime", by = by_colls]
  
  }
  
  
  
  if((sum(Rec_period == 0) != length(Rec_period)) & Aggregate & exists("tmpname")){
    
    tmpname3 <- tempfile(pattern = "persontime", tmpdir = tempdir(), fileext = ".rds")
    saveRDS(Dataset[, lapply(.SD, sum), .SDcols = "Persontime", by = by_colls], tmpname3)
    
    
  }
  
  ###
  
  #Recurrent events
  ####
  
  #Situation where results are be aggregated and all rec_periods are 0 
  ####
  
  if(Aggregate == T & exists("tmpname") & (sum(Rec_period == 0) == length(Rec_period))){
  
    if(print)print("Calculate aggregated resuts for recurrent events if only rec_periods of 0")   
    
    Dataset <- Dataset[, lapply(.SD, sum), .SDcols = "Persontime", by = by_colls]
    lapply(by_colls, function(x) Dataset[, eval(x) := as.character(get(x))])
    
    
      Events <- CalculateNumeratorAggregated(
        
            Dataset = readRDS(tmpname2)[, c(Person_id, Start_date, End_date, Birth_date, Strata), with = F],
            Person_id = Person_id,
            Start_date = Start_date, 
            End_date = End_date, 
            Dataset_events = readRDS(tmpname), 
            Agebands.file = Agebands_list, 
            Times.file = Dummy, 
            Name_event = Name_event, 
            Date_event = Date_event, 
            Birth_date = Birth_date, 
            Strata = by_colls
        
      )
      
      Dataset <- merge(x = Dataset, y = Events, by = by_colls, all.x = T )   
      Dataset[is.na(Dataset), ] <- 0
      lapply(Outcomes_rec[Rec_period == 0], function(x) Dataset[, eval(paste0("Persontime_", x)) := Persontime])
      
  }
  
  
  ####
  
  
  
  
  
  ###Situation where the results should not be aggregated.
  
  if(exists("tmpname") & !is.null(Outcomes_rec) & (sum(Rec_period == 0) != length(Rec_period)) |
     (exists("tmpname") & !is.null(Outcomes_rec) & (sum(Rec_period == 0) == length(Rec_period)) & Aggregate == F) 
     ){
    
    Dataset_events <- CleanOutcomes(Dataset = readRDS(tmpname), Person_id = "person_id", Rec_period = Rec_period, Outcomes = Outcomes_rec, Name_event = "name_event", Date_event = "date_event")

    
    if(print) print("Calculate recurrent events not aggregated")
  
    
    Dataset <- CalculateSubstractionDenominator(
      Dataset = Dataset,
      Start_date = Start_date,
      End_date = End_date,
      Dataset_events = Dataset_events,
      Person_id = Person_id,
      Name_event = Name_event,
      Date_event = Date_event,
      Outcomes_rec = Outcomes_rec,
      Rec_period = Rec_period,
      Aggregate = Aggregate,
      Strata = by_colls,
      Include_count = T,
      print = print
      
    )
    
    rm(Dataset_events)
    gc()
    
    #temp <- copy(Dataset)
    #Dataset <- copy(temp)
    
    colls <- Outcomes_rec[Rec_period != 0]
    colls <- colls[paste0("SUBTRCUM_", colls) %in% colnames(Dataset)]
    
    if(!Aggregate){
    lapply(colls, function(x)
      
      
      Dataset[, eval(paste0("Persontime_", x)) := 
                
                fifelse(!is.na(get(paste0("SUBTRCUM_",x))), 
                        get(End_date) - get(Start_date) + 1 - get(paste0("SUBTRCUM_",x)), 
                        Persontime)
                ][, eval(paste0("SUBTRCUM_",x)) := NULL]
      )
    }else{
      
      Dataset <- merge(x= readRDS(tmpname3), y = Dataset, by = by_colls, all.x = T)
      
      lapply(colls, function(x)
        
    
        Dataset[, eval(paste0("Persontime_", x)) := 
                  
                  fifelse(!is.na(get(paste0("SUBTRCUM_",x))), 
                          Persontime - get(paste0("SUBTRCUM_",x)), 
                          Persontime)
        ][, eval(paste0("SUBTRCUM_",x)) := NULL]
      )
      
      
      }
    
    rm(colls)
    
    #colls <- Outcomes_rec[Rec_period == 0]
    colls <- Outcomes_rec[!paste0("Persontime_", Outcomes_rec) %in% colnames(Dataset)]
    lapply(colls, function(x) Dataset[, eval(paste0("Persontime_", x)) := Persontime])
    
    rm(colls)
  }
  
  Dataset[is.na(Dataset), ] <- 0

  Outcomes <- c(Outcomes_nrec, Outcomes_rec)
  
  if(length(Outcomes) > 0 & exists("tmpname")){
    
    
    #colls <- colnames(Dataset)[grepl(pattern = paste0(paste0(Outcomes_rec,"_b"), collapse = "|"), colnames(Dataset))]
    
    
    B_MISSING <- Outcomes[!paste0(Outcomes,"_b") %in% unique(colnames(Dataset))]
    if(length(B_MISSING) > 0) lapply(paste0(B_MISSING,"_b"), function(x){Dataset <- Dataset[,eval(x) := 0]})
    
    P_MISSING <- Outcomes[!paste0("Persontime_",Outcomes) %in% unique(colnames(Dataset))]
    if(length(P_MISSING) > 0) lapply(paste0("Persontime_",P_MISSING), function(x){Dataset <- Dataset[,eval(x) := 0]})
    
    rm(P_MISSING, B_MISSING)
    
    
  }
  
  rm(Outcomes)
  
  Dataset <- Dataset[, coln, with=FALSE]
  setorderv(Dataset, sort_order)
  rm(sort_order)

  

  if(exists("tmpname")) if(file.exists(tmpname))  unlink(tmpname)
  if(exists("tmpname2")) if(file.exists(tmpname2))  unlink(tmpname2)
  if(exists("tmpname3")) if(file.exists(tmpname3))  unlink(tmpname3)
  
  
  return(Dataset)
  
  
  
  
  
  
  
  
  
  }
  
}
  
  