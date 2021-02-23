rm(list = ls(all=TRUE))
if (!require("data.table")) install.packages("data.table")
library(data.table)
path<-"Y:/Studies/ConcePTION/B_Documentation/2 Protocol_DSMB_Monitoring/WP 7.6 Data Characterization SAP/tmp_RE/Coding/RE_CountPersonTime/V9"

r_file<-"CountPersonTimeV9.R"
empty <- T

        
source(paste0(path,"/",r_file))

#Input_file<-fread(paste0(path,"/input_old.csv"), sep = ",")
Input_file1<-fread(paste0(path,"/input_events.csv"), sep = ",")
Input_file2<-fread(paste0(path,"/input_times.csv"), sep = ",")

date_cols<-c("start_date","end_date","date_birth")
Input_file2[,(date_cols) := lapply(.SD,function(x) (as.character(x))),.SDcols=date_cols]
Input_file2[,(date_cols) := lapply(.SD,function(x) (as.Date(x,"%Y%m%d"))),.SDcols=date_cols]
Input_file1[,date_event := as.character(date_event)]
Input_file1[,date_event:= as.Date(date_event,"%Y%m%d")]



#Input_file2[2,4]<-NA

Output_file1<-CountPersonTime(
  
  Dataset_events = Input_file1, 
  Dataset = Input_file2,
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
  Increment="year",
  Outcomes =c("outcome1","outcome2"),
  Unit_of_age = "year",
  include_remaning_ages = T,
  Aggregate = F
)



fileevents2<-fread(paste0(path,"/input_eventsD.csv"), sep = "," )
filepop2<-fread(paste0(path,"/input_popD.csv"), sep = ",")
library(lubridate)

date_format="%d%m%Y"
fileevents2[,date_event := as.Date(as.character(date_event), format = date_format)]
fileevents2[,date_event := as.Date(as.character(date_event))]
fileevents2[,date_event := dmy(date_event)]
filepop2[,datestart := dmy(datestart)]
filepop2[,dateend := dmy(dateend)]
filepop2[,birthdate := dmy(birthdate)]

if(empty==T) fileevents1 <- fileevents1[-1:-2,]

Output_file2<-CountPersonTime(
  Dataset_events = fileevents2, 
  Dataset = filepop2,
  Person_id = "id",
  Start_study_time = "20100101",
  End_study_time = "20191231",
  Start_date = "datestart", 
  End_date = "dateend",
  Birth_date = "birthdate",
  Strata = c("job","gender"),
  Name_event = "name_event",
  Date_event = "date_event",
  Age_bands = c(0,17,44,64),
  Increment="month",
  Outcomes =c("event1"),
  Unit_of_age = "year",
  include_remaning_ages = T,
  Aggregate = F
)


fileevents3<-fread(paste0(path,"/input_events3.csv"), sep = ",")
filepop3<-fread(paste0(path,"/input_pop3.csv"), sep = ",")



date_format="%d%m%Y"
#fileevents[,date_event := as.Date(as.character(date_event), format = date_format)]
#fileevents[,date_event := as.Date(as.character(date_event))]
fileevents3[,date_event := dmy(date_event)]
filepop3[,datestart := dmy(datestart)]
filepop3[,dateend := dmy(dateend)]
filepop3[,birthdate := dmy(birthdate)]

if(empty==T) fileevents3 <- fileevents3[-1:-2,]


Output_file3<-CountPersonTime(
  Dataset_events = fileevents3, 
  Dataset = filepop3,
  Person_id = "id",
  Start_study_time = "20100101",
  End_study_time = "20191231",
  Start_date = "datestart", 
  End_date = "dateend",
  Birth_date = "birthdate",
  Strata = c("job","gender"),
  Name_event = "name_event",
  Date_event = "date_event",
  Age_bands = c(0,17,44,64),
  Increment="year",
  Outcomes =c("event1","event2","event3"),
  Unit_of_age = "year",
  include_remaning_ages = T,
  Aggregate = F
)





load(paste0(path,"/D4_study_population.RData")) # fread(paste0(dirinput,"PERSONS.csv"))
filepop4 <- as.data.table(D4_study_population)

load(paste0(path,"/datasetOUTCOMES_2.RData")) 
fileevents4 <- as.data.table(datasetOUTCOMES)



Output_file4<-CountPersonTime(
  Dataset_events = fileevents4, 
  Dataset = filepop4,
  Person_id = "person_id",
  Start_study_time = "20170101",
  End_study_time = "20191231",
  Start_date = "study_entry_date",
  End_date = "study_exit_date",
  Birth_date = "date_of_birth",
  Strata = c("sex"),
  Name_event = "name_event",
  Date_event = "date_event",
  Age_bands = c(0,19,29,39,49,59,69,79),
  Increment="year",
  Outcomes =c("CAD_narrow", "CAD_broad","ACUASEARTHRITIS_broad", "DM_broad" , "HF_narrow","HF_broad","ARR_narrow","ARR_broad", "MYOCARD_narrow", "MYOCARD_broad", "COAGDIS_narrow", "COAGDIS_broad", "ALI_narrow", "ALI_broad", "AKI_broad", "GENCONV_broad", "ANAPHYL_broad", "COVID_broad" ),
  Unit_of_age = "year",
  include_remaning_ages = T,
  Aggregate = F
)




fileevents5<-fread(paste0(path,"/input_events4.csv"), sep = "," )



filepop5<-fread(paste0(path,"/input_pop4.csv"), sep = ";")





date_format="%d%m%Y"
fileevents5[,date_event := dmy(date_event)]
filepop5[,datestart := dmy(datestart)]
filepop5[,dateend := dmy(dateend)]
filepop5[,birthdate := dmy(birthdate)]



if(empty==T) fileevents5 <- fileevents5[-1:-2,]

Output_file5<-CountPersonTime(
  Dataset_events = fileevents5, 
  Dataset = filepop5,
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
  Increment="month",
  Outcomes =c("event1"),
  Unit_of_age = "year",
  include_remaning_ages = T,
  Aggregate = F
)

test change 22

