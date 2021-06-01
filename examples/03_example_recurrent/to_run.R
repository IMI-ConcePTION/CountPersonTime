#-------------------------------
# example 3: recurrent events 

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
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)


#load input
input_events<-fread(paste0(thisdir,"/input/input_events.csv"), sep = ",")
input_pop<-fread(paste0(thisdir,"/input/input_times.csv"), sep = ",")

date_cols<-c("start_date","end_date","date_birth")
input_pop[,(date_cols) := lapply(.SD,function(x) (as.character(x))),.SDcols=date_cols]
input_pop[,(date_cols) := lapply(.SD,function(x) (as.Date(x,"%Y%m%d"))),.SDcols=date_cols]
input_events[,date_event := as.character(date_event)]
input_events[,date_event:= as.Date(date_event,"%Y%m%d")]


View(input_pop)
View(input_events)

Output_file_recurrent <- CountPersonTime(
  Dataset_events = input_events, 
  Dataset = input_pop,
  Person_id = "person_id",
  Start_study_time = "20170101",
  End_study_time = "20171231",
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
  Aggregate = F,
  Rec_events = T,
  Rec_period = c(60,60)
)

View(Output_file_recurrent)

Output_file_recurrent_2 <- CountPersonTime(
  Dataset_events = input_events, 
  Dataset = input_pop,
  Person_id = "person_id",
  Start_study_time = "20190101",
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
  Aggregate = F,
  Rec_events = T,
  Rec_period = c(60,60)
)

View(Output_file_recurrent_2)

