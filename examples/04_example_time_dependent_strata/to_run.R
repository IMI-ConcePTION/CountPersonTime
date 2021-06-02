#-------------------------------
# example 4: time-dependent strata 
# person PERS0001 entered the study on the day of a monthly subscription to a online fitness program on 15 January 2020. Time was split in months in the variable month_after_start_subscription, as follows

# person_id,start_date,end_date,date_birth,sex,month_after_start_subscription
# PERS0001,20200115,20200214,19500325,1,1
# PERS0001,20200215,20200314,19500325,1,2
# PERS0001,20200315,20200414,19500325,1,3
# PERS0001,20200415,20200514,19500325,1,4
# PERS0001,20200515,20200614,19700501,1,5

# P0001 experienced  outcome1 before entering the study and outcome2 at the beginning of April, during month 3 after start subscription. Both outcomes were considered non-recurrent. Therefore, P0001 contributed no person time to the study of outcome1, and 2 months and 21 days to the study of outcome2: 31 days to subscription month 1, with no outcome, 29 days to subscription month 2, with no outcome, and 21 days to subscription month 3, with 1 outcome.

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

Output_file_timedependent <- CountPersonTime(
  Dataset_events = input_events, 
  Dataset = input_pop,
  Person_id = "person_id",
  Start_study_time = "20200101",
  End_study_time = "20201231",
  Start_date = "start_date", 
  End_date = "end_date",
  Birth_date = "date_birth",
  Strata = c("month_after_start_subscription"),
  Name_event = "name_event",
  Date_event = "date_event",
  Increment="year",
  Outcomes =c("outcome1","outcome2"),
  Aggregate = F
  )

View(Output_file_timedependent)
