
#rm(list = ls(all=TRUE))

###load subfunctions. These functions represent the relevant processes that are taken. It are copies of codes used within the the function.

#source("C:/CreateBands.R")
source("C:/CleanOutcomes.R")
source("C:/CreateAgebandIntervals.R")
source("C:/CreareTimeIntervals.R")
source("C:/CalculateNumeratorAggregated.R") #This is new created from code in level3 visits.It calulates numerator without stretching out the file first 


###

if (!require("data.table")) install.packages("data.table")


#Inputs
####
load("C:/test.Rdata")
#Dataset <- readRDS("C:/Dataset.rds")
Dataset_events <- readRDS( "C:/Events_original.rds")
Outcomes_rec <- c("outcome1","outcome2")
Rec_period <- c(10,5) 
Start_date = "start_date"
End_date = "end_date"
Strata = c("sex","city", "Ageband", "year")
Name_event = "name_event"
Date_event = "date_event"
Increment = INC
Aggregate = T
Person_id = "person_id"
Dataset_original <- readRDS("C:/Dataset_original.rds")
Age_bands = c(0,17,44,64)
Birth_date = "date_birth"
Start_study_time = "20150101"
End_study_time = "20191231"


###Preparing steps

#Set dates to date format
Start_study_time<-as.IDate(as.character(Start_study_time),"%Y%m%d")
End_study_time<-as.IDate(as.character(End_study_time),"%Y%m%d")


#Remove all events within lag time/wash out period/censoring period if there are recurrent events
Dataset_events <- CleanOutcomes(Dataset = Dataset_events, Person_id = "person_id", Rec_period = Rec_period, Outcomes = Outcomes_rec, Name_event = Name_event, Date_event = Date_event)

#Create file with the age bands and the age interval belonging to that. This is used for joining with the aim to assign labels 
Agebands <- CreateAgebandIntervals(Age_bands, include = T)

#Create a file with the relevant time intervals like with age bands.This is used for joining with the aim to assign labels 
Dummy <- CreareTimeIntervals(Start_study_time = Start_study_time, End_study_time = End_study_time, Increment = Increment)

###START CountNumeretorLean
##############

TestFastNumerator <- CalculateNumeratorAggregated(
  
  Dataset = Dataset, 
  Start_date = Start_date, 
  End_date = End_date, 
  Dataset_events = Dataset_events, 
  Agebands.file = Agebands, 
  Times.file = Dummy, 
  Name_event ="name_event", 
  Date_event = "date_event", 
  Birth_date = "date_birth", 
  Strata = c("sex","city")
  
)


#temp <- TestFastNumerator[, year1 := year(year)][, year := NULL][, year := as.character(year1)]
#temp <- TestFastNumerator[, day := as.character(day)]
#Output_file1 <- Output_file1[, day := as.character(day)]
#check <- merge(temp, Output_file1, by = Strata)[, x := fifelse(outcome1 == outcome1_b & outcome2 == outcome2_b, T, F) ]



#################


#START SUBRACT
################


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
