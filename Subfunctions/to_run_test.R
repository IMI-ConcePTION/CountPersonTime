

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

INC <- "day"

source("CleanOutcomes.R")
source("CreateAgebandIntervals.R")
source("CreateTimeIntervals.R")
source("CheckAndPrepareDates.R")
source("CalculateSubtractionDenominator.R")
source("CalculateNumeratorAggregated.R")

test <- c("CountPersonTimeV13.8.R", "CountPersonTimeV13.9.R")

for(i in 1:length(test)){

source(paste0(thisdir,"/",test[i]))  

    
print(peakRAM(assign(paste0("test",i,"_output"), CountPersonTime(
  
  #Dataset_events = EVENTS1, 
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
  #Outcomes_rec = c("outcome1", "outcome2"),
  Unit_of_age = "year",
  include_remaning_ages = F,
  Aggregate = F,
  #Rec_period = c(10, 10),
  save_intermediate = paste0("C:/TEST",i,".Rdata"),
  load_intermediate = F,
  check_overlap = F,
  print = F
)
  
)))


}


#load(paste0("C:/TEST",1,".Rdata"))
#test1_intermediate <- Dataset
#load(paste0("C:/TEST",2,".Rdata"))
#test2_intermediate <- Dataset

#rm(Dataset)


prepare <- function(file, cols){

file<- copy(file)[, cols, with = F]  
lapply(cols, function(x) file[, eval(x) := as.character(get(x))])
setorderv(file, cols)
}

#test1_output <- prepare(test1_output, colnames(test1_output))
#test2_output <- prepare(test2_output, colnames(test1_output))



#compare2 <-  test1_output == test2_output
#test2 <- sum(compare2==F)
#test2.1 <- sum(is.na(compare2))



peakRAM(set1 <- test2_output[person_id %in% unique(EVENTS1$person_id),])
peakRAM(set2 <- test2_output[!person_id %in% unique(EVENTS1$person_id),])

peakRAM(SUB <- CalculateSubstractionDenominator(
  
  Dataset = set1,
  Start_date = "start_date",
  End_date = "end_date",
  Dataset_events = EVENTS1,
  Person_id = "person_id",
  Name_event = "name_event",
  Date_event = "date_event",
  Outcomes_rec = c("outcome1", "outcome2"),
  Rec_period = c(10, 10),
  Aggregate = T,
  Strata = c("sex","city", "Ageband", INC),
  Include_count = T,
  print = F
  
))





