
rm(list = ls(all=TRUE))


if (!require("rstudioapi")) install.packages("rstudioapi")

thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(thisdir)
###load subfunctions. These functions represent the relevant processes that are taken. It are copies of codes used within the the function.

#source("C:/CreateBands.R")
source("CleanOutcomes.R")
source("CreateAgebandIntervals.R")
source("CreareTimeIntervals.R")
source("CalculateSubtractionDenominator.R")  
source("CalculateNumeratorAggregated.R") #This is new created from code in level3 visits.It calulates numerator without stretching out the file first
source("CheckAndPrepareDates.R")
###

if (!require("data.table")) install.packages("data.table")


#Inputs
####
load("C:/test.Rdata")
#Dataset <- readRDS("C:/Dataset.rds")
Dataset_events <- readRDS( "C:/Events_original.rds")
Dataset_original <- readRDS("C:/Dataset_original.rds")
Age_bands = c(0,17,44,64)
Start_study_time = "20150101"
End_study_time = "20191231"
Increment = "month"

###Preparing steps

#Set dates to date format
Start_study_time<-as.IDate(as.character(Start_study_time),"%Y%m%d")
End_study_time<-as.IDate(as.character(End_study_time),"%Y%m%d")


#Remove all events within lag time/wash out period/censoring period if there are recurrent events
peakRAM(Dataset_events <- CleanOutcomes(Dataset = Dataset_events, Person_id = "person_id", Rec_period = c(10,0), Outcomes = c("outcome1", "outcome2"), Name_event = "name_event", Date_event = "date_event"))
Dataset_events <- Dataset_events[, Iteration := NULL]

#Create file with the age bands and the age interval belonging to that. This is used for joining with the aim to assign labels 
Agebands <- CreateAgebandIntervals(Age_bands, include = T)

#Create a file with the relevant time intervals like with age bands.This is used for joining with the aim to assign labels 
Dummy <- CreateTimeIntervals(Start_study_time = Start_study_time, End_study_time = End_study_time, Increment = Increment)

###START CountNumeretorLean
##############

peakRAM(TestFastNumerator <- CalculateNumeratorAggregated(
  
  Dataset = Dataset_original,
  Person_id = "person_id",
  Start_date = "start_date", 
  End_date = "end_date", 
  Dataset_events = Dataset_events, 
  Agebands.file = Agebands, 
  Times.file = Dummy, 
  Name_event ="name_event", 
  Date_event = "date_event", 
  Birth_date = "date_birth", 
  Strata = c("sex","city")
  
)
)

#temp <- TestFastNumerator[, year1 := year(year)][, year := NULL][, year := as.character(year1)]
#temp <- TestFastNumerator[, day := as.character(day)]
#Output_file1 <- Output_file1[, day := as.character(day)]
#check <- merge(temp, Output_file1, by = Strata)[, x := fifelse(outcome1 == outcome1_b & outcome2 == outcome2_b, T, F) ]



#################


#START SUBRACT
################

#load("C:/test.Rdata")

peakRAM(test <- CalculateSubstractionDenominator(
  
  
  Dataset = Dataset,
  Start_date = "start_date",
  End_date = "end_date",
  Dataset_events = Dataset_events,
  Person_id = "person_id",
  Name_event = "name_event",
  Date_event = "date_event",
  Outcomes_rec = c("outcome1", "outcome2"),
  Rec_period = c(10, 0),
  Aggregate = T,
  Strata = c("sex","city", "Ageband", "month"),
  Include_count = T
  
))


##############



#SUBTRCUM_MISSING <- Outcomes_rec[!paste0("SUBTRCUM_",Outcomes_rec) %in% unique(colnames(Dataset))] 
#if(length(SUBTRCUM_MISSING) > 0)lapply(paste0("SUBTRCUM_",SUBTRCUM_MISSING), function(x){Dataset <- Dataset[,eval(x) := 0]})

#B_MISSING <- Outcomes_rec[!paste0(Outcomes_rec,"_b") %in% unique(colnames(Dataset))] 
#if(length(B_MISSING) > 0) lapply(paste0(B_MISSING,"_b"), function(x){Dataset <- Dataset[,eval(x) := 0]})


    