
rm(list = ls(all=TRUE))

CleanOutcomes <- function(Dataset = NULL, Person_id, Rec_period = NULL, Outcomes = NULL, Name_event = NULL, Date_event = NULL){
  
  if(length(Outcomes) != length(Rec_period)) stop("Specifiy the same number of Rec_periods as Outcomes")
  
  Dataset  <- copy(Dataset)[get(Name_event) %in% Outcomes]
  tmp <- copy(Dataset[0])
  
  for (i in 1:length(Outcomes)){
    
    
    print(paste("Remove ",Outcomes[i], "outcomes (conditions or vaccines) within a the Rec_period distance of ",Rec_period[i]," days"))
    Dataset_temp  <- copy(Dataset)[get(Name_event) == Outcomes[i],]
    
    it=1
    while(nrow(Dataset_temp) > 0){ 
      
      setorderv(Dataset_temp,c(Person_id,Name_event,Date_event))
      Dataset_temp <- Dataset_temp[,D := shift(get(Date_event)),by = c(Person_id,Name_event) ]
      Dataset_temp <- Dataset_temp[,dif := get(Date_event) - D]
      Dataset_temp <- Dataset_temp[is.na(dif), dif := 0 ][,dif := as.numeric(dif)]
      Dataset_temp <- Dataset_temp[,cumdif := cumsum(dif),by = c(Person_id,Name_event)]
      
      Dataset_temp2 <- Dataset_temp[ cumdif <= Rec_period[i],]
      setorderv(Dataset_temp2,c(Person_id,Name_event,Date_event))
      Dataset_temp2 <- Dataset_temp2[,.SD[1],by = c(Person_id,Name_event)][,Iteration := it]
      
      tmp <- rbindlist(list(tmp, Dataset_temp2),fill=T)
      rm(Dataset_temp2)
      
      Dataset_temp <- Dataset_temp[cumdif > Rec_period[i],]
      
      lapply(c("dif","cumdif","D"), function(x){Dataset_temp <-Dataset_temp[,eval(x) := NULL]})
      print(paste0("Cycle ",it))
      it=it+1
      gc()
    }
    
    
    
    rm(Dataset_temp, it)
    gc()
    
    
  }  
  lapply(c("dif","cumdif","D"), function(x){tmp <- tmp[,eval(x) := NULL]})
  setorderv(tmp,c(Person_id,Name_event,Date_event))
  return(tmp)
  
} 



if (!require("data.table")) install.packages("data.table")


#Inputs
####
load("C:/test.Rdata")
#Dataset <- readRDS("C:/Dataset.rds")
Dataset_events <- readRDS( "C:/Dataset_events.rds")
Outcomes_rec <- c("outcome1","outcome2", "outcome3")
Rec_period <- c(10,10,10) 
Start_date = "start_date"
End_date = "end_date"
Strata = c("sex","city", "Ageband", "year")
Name_event = "name_event"
Date_event = "date_event"
Increment = "year"
Aggregate = T
Person_id = "person_id"



Dataset_events <- CleanOutcomes(Dataset = Dataset_events, Person_id = "person_id", Rec_period = Rec_period, Outcomes = Outcomes_rec, Name_event = Name_event, Date_event = Date_event)


#START
############


#Standardize names because data.table gives problems in some occasions when working with varaibles as inputs
setnames(Dataset_events, c(eval(Person_id), eval(Name_event), eval(Date_event)), c("ID","EVNT", "DTEVNT"))
setnames(Dataset, c(eval(Person_id), eval(Start_date), eval(End_date)), c("ID", "ST", "EN"))

#Select events of interest based on start and end dates also called persontime
if(!Aggregate) Dataset_temp <- copy(Dataset)[ID %in% unique(Dataset_events[["ID"]]),][, c("ID", "ST", "EN"), with = F]
if(Aggregate) Dataset_temp <- copy(Dataset)[ID %in% unique(Dataset_events[["ID"]]),][, c("ID", "ST", "EN", Strata), with = F]

Dataset_events <- Dataset_events[, dummy := DTEVNT]
setkeyv(Dataset_temp, c("ID", "ST", "EN"))
Dataset_events_count <- foverlaps(Dataset_events, Dataset_temp, by.x = c("ID","DTEVNT","dummy"), nomatch = 0L, type = "any")


#Get counts recurrent events
############

####
#Dataset_events_rec3 <- Dataset_events_rec2[,.("b" = .N), by = c("ID", "ST", "EN", Name_event)][,var := paste0(get(Name_event),"_b")]


if(!Aggregate){
  Dataset_events_count <- dcast(Dataset_events_count[, var2 := 1], ID + ST + EN ~  EVNT, value.var = "var2", fill = 0, fun.aggregate = sum)
  setnames(Dataset_events_count, Outcomes_rec, paste0(Outcomes_rec,"_b"))
  Dataset <- merge(x = Dataset, y = Dataset_events_count, by = c("ID", "ST", "EN"), allow.cartesian = F, all.x = T, all.y = F)
}

if(Aggregate) {Dataset1 <- data.table::dcast(Dataset_events_count[, var2 := 1], formula(paste0(paste(Strata, collapse = " + "), " ~  EVNT")) , value.var = "var2", fun.aggregate = sum)
  #setnames(Dataset1, Outcomes_rec, paste0(Outcomes_rec,"_b"))
}
rm(Dataset_events_count)
gc()

####



###########



Outcomes_rec1 <-  Outcomes_rec[ Outcomes_rec  %in% Outcomes_rec[!Rec_period == 0]]  
Outcomes_rec0 <-  Outcomes_rec[! Outcomes_rec %in% Outcomes_rec1] 

#Dataset_events_rec0  <- copy(Dataset_events)[EVNT %in% Outcomes_rec0]
Dataset_events_rec1  <- copy(Dataset_events)[EVNT %in% Outcomes_rec1]

Rec_period1 <- Rec_period[Outcomes_rec %in% Outcomes_rec1]

#rm(Dataset_events)
gc()

#Calculate for every observation period the amount of persontime that needs to subtracted for every event
################################################################################################################################


if(nrow(Dataset_events_rec1) > 0){
  
  print("Calculate start and end dates for the censoring periods")
  
  for(i in 1:length(Rec_period1)){
    
    print(paste("Set censoring start and end dates for outcome ",Outcomes_rec1[i], " with a duration of ",Rec_period1[i]," days"))
    #Dataset_events_rec1 <- Dataset_events_rec1[dif != 0 & get(Name_event) == Outcomes_rec1[i] & dif < Rec_period1[i], Delete := T ][is.na(Delete),]
    Dataset_events_rec1 <- Dataset_events_rec1[EVNT == Outcomes_rec1[i], ":=" (RecSTDT = DTEVNT, RecENDT = DTEVNT + Rec_period1[i])]  
    gc()
    
  }
  
  print("Calculate days to subtract from persontime for recurrent events")
  
  #if(!Aggregate) Dataset_temp <- copy(Dataset)[ID %in% unique(Dataset_events_rec1[["ID"]]),][, c("ID", "ST", "EN"), with = F]
  #if(Aggregate) Dataset_temp <- copy(Dataset)[ID %in% unique(Dataset_events_rec1[["ID"]]),][, c("ID", "ST", "EN", Strata), with = F]
  
  setkeyv(Dataset_temp, c("ID", "ST", "EN"))
  Dataset_events_rec2 <- foverlaps(Dataset_events_rec1, Dataset_temp, by.x = c("ID","RecSTDT","RecENDT"), nomatch = 0L, type = "any")
  
  if(nrow(Dataset_events_rec2) > 0){
    
    Dataset_events_rec2 <- Dataset_events_rec2[, row2 := row.names(Dataset_events_rec2)]
    Dataset_events_rec2 <- Dataset_events_rec2[,':=' (start_date2 = max(ST, RecSTDT, na.rm = T), end_date2 = min(EN + 1, RecENDT, na.rm = T)), keyby = row2]     
    Dataset_events_rec2 <- Dataset_events_rec2[, SUBTR := (as.numeric(end_date2) - as.numeric(start_date2)),by = row2]
    
    Dataset_events_rec2 <- Dataset_events_rec2[, var := paste0("SUBTRCUM_", EVNT)]
    
    if(Aggregate) Dataset2 <- data.table::dcast(Dataset_events_rec2, formula(paste0(paste(Strata, collapse = " + "), " ~  var")) , value.var = "SUBTR", fun.aggregate = sum)
  
    if(!Aggregate){
      Dataset_events_rec2 <- data.table::dcast(Dataset_events_rec2, ID + ST + EN ~  var, value.var = "SUBTR", fun.aggregate = sum)
      Dataset <- merge(x = Dataset, y = Dataset_events_rec2 , by = c("ID", "ST", "EN"), all.x = T , all.y = F, allow.cartesian = F)
    }
    
  }else{
    
    print("All recurrent events fall witout the period of interest")
  }
  
  rm(Dataset_temp)  
  gc()
}


rm(Dataset_events_rec1, Dataset_events_rec2)
gc()

if(Aggregate){
Dataset <- merge(Dataset1, Dataset2, all = T, by = Strata)
rm(Dataset1, Dataset2)
gc()    
}



#Check if all outcomes are present. If not add column with value 0
    

SUBTRCUM_MISSING <- Outcomes_rec[!paste0("SUBTRCUM_",Outcomes_rec) %in% unique(colnames(Dataset))] 
if(length(SUBTRCUM_MISSING) > 0)lapply(paste0("SUBTRCUM_",SUBTRCUM_MISSING), function(x){Dataset <- Dataset[,eval(x) := 0]})

B_MISSING <- Outcomes_rec[!paste0(Outcomes_rec,"_b") %in% unique(colnames(Dataset))] 
if(length(B_MISSING) > 0) lapply(paste0(B_MISSING,"_b"), function(x){Dataset <- Dataset[,eval(x) := 0]})


    
################################################################################################################################
    
#Calculate the number of events per observation period
################################################################################################################################
    
    

    
    ################################################################################################################################   
  #}else{
  #  lapply(Outcomes_rec, function(x){Dataset <- Dataset[,eval(paste0(x, "_b")) := 0]})
  #  lapply(Outcomes_rec, function(x){Dataset <- Dataset[,eval(paste0("SUBTRCUM_",Outcomes_rec)) := 0]})
    
  #}
#}
