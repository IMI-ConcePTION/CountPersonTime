#-------------------------------
# example 1: regular use of the function 

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#load function
source(paste0(thisdir,"/../../CountPersonTimeV12.4.R"))

# load data.table
if (!require("data.table")) install.packages("data.table")
library(data.table)


#load input
Input_file1<-fread(paste0(thisdir,"/input/input_events.csv"), sep = ",")
Input_file2<-fread(paste0(thisdir,"/input/input_times.csv"), sep = ",")

date_cols<-c("start_date","end_date","date_birth")
Input_file2[,(date_cols) := lapply(.SD,function(x) (as.character(x))),.SDcols=date_cols]
Input_file2[,(date_cols) := lapply(.SD,function(x) (as.Date(x,"%Y%m%d"))),.SDcols=date_cols]
Input_file1[,date_event := as.character(date_event)]
Input_file1[,date_event:= as.Date(date_event,"%Y%m%d")]


Output_file <- CountPersonTime(
  Dataset_events = Input_file1, 
  Dataset = Input_file2,
  Person_id = "person_id",
  Start_study_time = "20120101",
  End_study_time = "20191231",
  Start_date = "start_date", 
  End_date = "end_date",
  Birth_date = "date_birth",
  Strata = c("sex","city"),
  Name_event = "name_event",
  Date_event = "date_event",
  Age_bands = c(0,17,44,64),
  Increment="year",
  Outcomes =c("outcome1","outcome2"),
  Unit_of_age = "year",
  include_remaning_ages = T,
  Aggregate = F
)

View(Output_file)