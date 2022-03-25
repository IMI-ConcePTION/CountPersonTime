
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
  
  
  if(any(Outcomes_rec %in% Outcomes_nrec)){stop("Overlapping event names for Outcomes_rec and Outcomes_nrec")}
  
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
    
    CheckAndPrepareDates(
      Dataset = Dataset, 
      Person_id = Person_id, 
      Start_study = Start_study_time, 
      End_study = End_study_time, 
      Start_date = Start_date,
      End_date = End_date, 
      Birth_date = Birth_date, 
      Age_bands = Age_bands,
      Increment = Increment, 
      print = print, 
      check_overlap = check_overlap
      
      
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
        Agebands_list <-  CreateAgebandIntervals(Age_bands, include = T)
      
        #Merge the overlapping
        setkeyv(Dataset, c("age_start","age_end"))
        Dataset <- foverlaps(Agebands_list, Dataset, by.x = c("ST","EN"), nomatch = 0L, type = "any")
        
        # select the rows that doubled by the merge. In these, multiple agebands occur witing the obeservation period. So start and end dated need to be adapted
        Dataset <- Dataset[, row := row.names(Dataset)]
        Dataset <- Dataset[age_start < ST  ,eval(Start_date) := as.IDate(add_with_rollback(get(Birth_date), period(ST,units = Unit_of_age), roll_to_first = T, preserve_hms = T)), by = row]
        Dataset <- Dataset[age_end > EN  ,eval(End_date) := as.IDate(add_with_rollback(get(Birth_date), period(EN + 1,units = Unit_of_age), roll_to_first = T, preserve_hms = T)) - 1, by = row]
        Dataset <- Dataset[,':=' (age_start = NULL, age_end = NULL,ST = NULL, EN = NULL, row = NULL)]
        }
    }
  
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
  
  return(Dataset)
  
  if (!missing(save_intermediate)) {
    save(Dataset, file = save_intermediate)
  } 
  
  }
  
}
  
  