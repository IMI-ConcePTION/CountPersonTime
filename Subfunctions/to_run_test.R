

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
source("SetToInteger.R")


test <- c("CountPersonTimeV13.8.R", "CountPersonTimeV13.9.R")

#Settings
Aggregate <- F



for(i in 1:length(test)){

source(paste0(thisdir,"/",test[i]))  

    
print(peakRAM(CountPersonTime(
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
  Aggregate = Aggregate,
  #Rec_period = c(10, 10),
  save_intermediate = paste0("D:/TEST",i,".Rdata"),
  load_intermediate = F,
  check_overlap = F,
  print = F
)
  
))


}


load(paste0("D:/TEST",1,".Rdata"))
test1_intermediate <- Dataset
load(paste0("D:/TEST",2,".Rdata"))
test2_intermediate <- Dataset

#rm(Dataset)

prepare <- function(file, cols){

file<- copy(file)[, cols, with = F]  
lapply(cols, function(x) file[, eval(x) := as.character(get(x))])
setorderv(file, cols)
}

test1_intermediate <- prepare(test1_intermediate, colnames(test1_intermediate))[, month := substr(month, 1 , 7)]
test2_intermediate <- prepare(test2_intermediate, colnames(test1_intermediate))

compare1 <-  test1_intermediate == test2_intermediate
test1 <- sum(compare1==F)
test1.1 <- sum(is.na(compare1))

rm(test1_intermediate, test2_intermediate, compare1,  Dataset)
gc()

for(i in 1:length(test)){
  
  source(paste0(thisdir,"/",test[i]))  
  
  
  print(peakRAM(assign(paste0("test",i,"_output"), CountPersonTime(
    Dataset_events = EVENTS1, 
    #Dataset = PERIODS1,
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
    save_intermediate = paste0("D:/TEST",i,".Rdata"),
    load_intermediate = T,
    check_overlap = F,
    print = F
  )
  
)))
  
  
}


test1_output <- prepare(test1_output, colnames(test1_output))
test2_output <- prepare(test2_output, colnames(test1_output))



compare2 <-  test1_output == test2_output
test2 <- sum(compare2==F)
test2.1 <- sum(is.na(compare2))


# 
# peakRAM(set1 <- test2_output[person_id %in% unique(EVENTS1$person_id),])
# peakRAM(set2 <- test2_output[!person_id %in% unique(EVENTS1$person_id),])
# rm(Dataset)
# 
# mergeback <- colnames(set1)[!colnames(set1) %in% c("Persontime")]
# 
# peakRAM(SUB <- CalculateSubstractionDenominator(
#   
#   Dataset = set1[, ..mergeback],
#   Start_date = "start_date",
#   End_date = "end_date",
#   Dataset_events = EVENTS1,
#   Person_id = "person_id",
#   Name_event = "name_event",
#   Date_event = "date_event",
#   Outcomes_rec = c("outcome1", "outcome2"),
#   Rec_period = c(10, 10),
#   Aggregate = F,
#   Strata = c("sex","city", "Ageband", INC),
#   Include_count = T,
#   print = F
#   
# ))
# 
# #if(Aggregate){}
# peakRAM(set1 <- set1[, .(Persontime = sum(Persontime)) , by = c("sex","city", "Ageband", INC)])
# peakRAM(set2 <- set2[, .(Persontime = sum(Persontime)) , by = c("sex","city", "Ageband", INC)])
# mergeback <- mergeback[!mergeback %in% c(Start_date, End_date, Person_id)]
# #}
# 
# peakRAM(aatest <-  merge(set1, SUB, by = mergeback, all.x = T))
# 
# peakRAM(aatest2 <- rbindlist(list(aatest, set2), fill = T, use.names = T))
# 
# rm(mergeback, set1, set2)
# 
# 
# 
# 
# 
# 
# 
