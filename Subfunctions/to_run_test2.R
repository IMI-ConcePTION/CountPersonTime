


rm(list = ls(all=TRUE))

library("peakRAM")
if (!require("rstudioapi")) install.packages("rstudioapi")
if (!require("data.table")) install.packages("data.table")
#library(peakRAM)

thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#r_file<-"CountPersonTimeV13.9.R"
#r_file<-"CountPersonTimeV13.8.R"



EVENTS1 <- readRDS("C:/Users/relbers/Documents/GitHub/CoutPersonTime/big_events.rds")
PERIODS1 <- readRDS("C:/Users/relbers/Documents/GitHub/CoutPersonTime/big_times.rds")

id <- sample(unique(PERIODS1$person_id),10000)

PERIODS1 <- PERIODS1[person_id %in% id,]
EVENTS1 <-  EVENTS1[person_id %in% id,]
#EVENTS1 <- readRDS(paste0(thisdir,"/EVENTS1.rds"))
#PERIODS1 <- readRDS(paste0(thisdir,"/PERIODS1.rds"))

INC <- "month"

source("CleanOutcomes.R")
source("CreateAgebandIntervals.R")
source("CreateTimeIntervals.R")
source("CheckAndPrepareDates.R")
source("CalculateSubtractionDenominator.R")
source("CalculateNumeratorAggregated.R")
source("SplitSpellsAgeBands.R")
source("CalculateNumeratorNotRecurrent.R")

test <- c("CountPersonTimeV13.8.R", "CountPersonTimeV13.9.R")

#Settings
Aggregate <- F



for(i in 1:length(test)){
  
  source(paste0(thisdir,"/",test[i]))  
  
  
  print(peakRAM(assign(paste0("test",i,"_output"), CountPersonTime(
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
    Outcomes_nrec = c("outcome1", "outcome2"),
    Unit_of_age = "year",
    include_remaning_ages = F,
    Aggregate = Aggregate,
    #Rec_period = c(10, 10),
    #save_intermediate = paste0("D:/TEST",i,".Rdata"),
    #load_intermediate = T,
    check_overlap = F,
    print = F
  )
  
  )))
  
  
}




#rm(Dataset)

prepare <- function(file, cols){
  
  file<- copy(file)[, cols, with = F]  
  lapply(cols, function(x) file[, eval(x) := as.character(get(x))])
  setorderv(file, cols)
}

test1_output <- prepare(test1_output, colnames(test1_output))
test2_output <- prepare(test2_output, colnames(test1_output))



compare2 <-  test1_output == test2_output
test2 <- sum(compare2==F)
test2.1 <- sum(is.na(compare2))
