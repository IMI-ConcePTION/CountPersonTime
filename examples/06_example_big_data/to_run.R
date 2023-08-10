#-------------------------------
# example 1: regular use of the function 

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#load function
source(paste0(thisdir,"/../../CountPersonTimeV14.0.R"))

# load data.table
if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)
if (!require("truncnorm")) install.packages("truncnorm")
library(truncnorm)

# Create dummy data
set.seed(123)
n_row <- 100
mean_periods_per_person <- 1.5

fake_ids <- sprintf("%010d", sample(seq_len(round(n_row/mean_periods_per_person)), n_row, replace = T))
possible_dates <- seq.Date(ymd(20000101), ymd(20220101), by = "day")
flag_ids <- sort(rep(seq_len(n_row), 2))

input_times <- data.table(person_id = paste0("PERS", fake_ids))
input_times <- rbind(input_times, input_times)
input_times[, start_end_date := sample(possible_dates, n_row * 2, replace = T)]
while (any(duplicated(input_times))) {
  input_times[duplicated(input_times), start_end_date := sample(possible_dates, sum(duplicated(input_times)), replace = T)]
}
setorder(input_times, person_id, start_end_date)
input_times[, flag := rep(c("start_date", "end_date"), n_row)]
input_times[, flag_ids := flag_ids]
input_times <- dcast(input_times, person_id + flag_ids ~ flag, value.var = "start_end_date")
input_times[, flag_ids := NULL]
setcolorder(input_times, c("person_id", "start_date", "end_date"))
input_times[, date_birth := min(start_date), by = "person_id"]

temp <- unique(copy(input_times)[, c("start_date", "end_date") := NULL])

input_events <- copy(temp)

temp[, offset := round(rtruncnorm(nrow(temp), a = 1, b = 30000,  mean = 10000, sd = 100000))]
temp[, sex := sample(1:2, nrow(temp), replace = T)][, city := sample(LETTERS, nrow(temp), replace = T)]

input_events[, date_birth := NULL][, n_events := round(rtruncnorm(nrow(input_events), a = 1, b = 20,  mean = 1, sd = 3))]
input_events <- input_events[rep(1:.N, n_events)]
input_events[, n_events := NULL][, date_event := sample(possible_dates, nrow(input_events), replace = T)]
input_events[, name_event := paste0("outcome", sample(seq_len(round(n_row ^ (1/3))), nrow(input_events), replace = T))]

input_times <- input_times[temp, on = c("person_id", "date_birth")]
input_times[, date_birth := date_birth - offset][, offset := NULL]

fwrite(input_events, paste0(thisdir,"/input/input_events.csv"))
fwrite(input_times, paste0(thisdir,"/input/input_times.csv"))
rm(temp, input_events, input_times)


#load input
Input_file1 <- fread(paste0(thisdir,"/input/input_events.csv"))
Input_file2 <- fread(paste0(thisdir,"/input/input_times.csv"))

standard <- CountPersonTime(
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
  Outcomes_nrec =c("outcome1","outcome2"),
  Unit_of_age = "year",
  include_remaning_ages = T,
  intermediate_folder = file.path(thisdir, "intermediate"),
  Aggregate = F
)
