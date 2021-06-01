#-------------------------------
# example 2: regular use of the function 

# 1)	Person 1 does not contribute at all person time for the event 1, because she had an event before entering the study.
# 2)	Person 1’s event does not be counted as an event
# 3)	Person 2 contributes to 2010 less than 365 days for event 1 (and for all events actually) because he entered the study in March

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))


#load function
source(paste0(thisdir,"/../../CountPersonTimeV12.4.R"))

# load data.table
if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)



dirinput <- paste0(thisdir,"/i_input/")

fileevents<-fread(paste0(dirinput,"input_events.csv"), sep = "," )
filepop<-fread(paste0(dirinput,"input_pop.csv"), sep = ",")


date_format="%d%m%Y"
#fileevents[,date_event := as.Date(as.character(date_event), format = date_format)]
#fileevents[,date_event := as.Date(as.character(date_event))]
fileevents[,date_event := dmy(date_event)]
filepop[,datestart := dmy(datestart)]
filepop[,dateend := dmy(dateend)]
filepop[,birthdate := dmy(birthdate)]

View(filepop)
View(fileevents)

Output_file<-CountPersonTime(
  Dataset_events = fileevents, 
  Dataset = filepop,
  Person_id = "id",
  Start_study_time = "20100101",
  End_study_time = "20191231",
  Start_date = "datestart", 
  End_date = "dateend",
  Birth_date = "birthdate",
  Strata = c("gender"),
  Name_event = "name_event",
  Date_event = "date_event",
  Age_bands = c(0,17,44,64),
  Increment="year",
  Outcomes =c("event1"),
  Unit_of_age = "year",
  include_remaning_ages = T,
  Aggregate = F
)

View(Output_file)
