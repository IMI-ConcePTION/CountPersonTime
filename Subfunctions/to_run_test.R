

rm(list = ls(all=TRUE))

library("peakRAM")
if (!require("rstudioapi")) install.packages("rstudioapi")
if (!require("data.table")) install.packages("data.table")
#library(peakRAM)

thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#r_file<-"CountPersonTimeV13.9.R"
#r_file<-"CountPersonTimeV13.8.R"



EVENTS1 <- readRDS(paste0(thisdir,"/EVENTS1.rds"))
PERIODS1 <- readRDS(paste0(thisdir,"/PERIODS1.rds"))

INC <- "month"

source("CleanOutcomes.R")
source("CreateAgebandIntervals.R")
source("CreateTimeIntervals.R")
source("CheckAndPrepareDates.R")
source("CalculateSubtractionDenominator.R")

test <- c("CountPersonTimeV13.8.R", "CountPersonTimeV13.9.R")

for(i in 1:length(test)){

source(paste0(thisdir,"/",test[i]))  

    
print(peakRAM(CountPersonTime(
  
  Dataset_events = EVENTS1, 
  Dataset = PERIODS1,
  Person_id = "person_id",
  Start_study_time = "20150101",
  End_study_time = "20191231",
  Start_date = "start_date", 
  End_date = "end_date",
  Birth_date = "date_birth",
  Strata = c("sex","city"),
  Name_event = "name_event",
  Date_event = "date_event",
  Age_bands = c(0,17,44,64),
  Increment = INC,
  Outcomes_rec = c("outcome1","outcome2"),
  Unit_of_age = "year",
  include_remaning_ages = T,
  Aggregate = F,
  Rec_period = c(10,10),
  save_intermediate = paste0("C:/TEST",i,".Rdata"),
  load_intermediate = F,
  check_overlap = F,
  print = F
)
  
))


}


load(paste0("C:/TEST",1,".Rdata"))
test1 <- Dataset
load(paste0("C:/TEST",2,".Rdata"))
test2 <- Dataset

rm(Dataset)

#test1 == test2

