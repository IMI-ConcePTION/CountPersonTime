# CountPersonTime
Calculation of persontime prior to events. 

# Context

CreatePersonTime is meant to be used in studies where incidence rates of one or more events need to be computed across years, or months, possibly across strata of categorical variables such as age band and/or sex. 

# Purpose
CreatePersonTime takes as input a dataset where disjoint observation windows per person are recorded with time-dependent variables. At the time when this function is called, all the persons meeting exclusion criteria (eg persons having had the event during look-back period) must have been already discarded, but the person time of persons in the study can be included interely. The function splits the person time of each person, discards person time that is outside of the study period, and labels the remaining period with calendar years (or months or days) according to (possibly time-dependent) categorical variables; moreover, it computes person time per each of the events of interest, by applying censoring after the first occurrence of the event, and indicates whether an event is observed.

# Note that 
-	Events occurred before the start of the study, or before the person enters the study, will indicate that that person should not contribute to person time at risk for that event, but they will not be counted as ‘events’
-	In particular if an event is after the start of the study but before the person enters the study, this event will not be counted as an observed event (because it happens before before the person enters the study) but it will be used to set to 0 the time that this subject contributes to the persp-time at risk for that event
Input

# Structure of input data: 
* a “time” dataset with
  * a person identifier
  * start and end date of validity of the information of each record
  * birth date of the person(optional); if multiple record for the same person are available, this date should be the same
  * a list of (possibly time-dependent) variables (optional)
* an “events” dataset with
  * the person identifier
  * a variable date
  * a variable containing name of the event

# Main parameters
-	Dataset_events (str): name of the dataset containing events: it must be a file in memory
-	Dataset (str): name of the dataset containing times: it must be a file in memory
-	Person_id (str): name of the variable containing the identifier of the person in both datasets
-	Start_study_time (str): date when the study starts, indicated as a string in format YYYYMMDD; if increment is ‘yearly’ this must be a January 1st, if  increment is ‘monthly’ this must be a 1st of the month,  if increment is ‘week’ and this is not a Monday, then the first week is lasting less than 7 days and the corresponding value in the output is marked with an asterisk
-	End_study_time (str): date when the study ends, indicated as a string in format YYYYMMDD; if increment is ‘year’ this must be a December 31st, if  increment is ‘month’ this must be a end of the month,  if increment is ‘week’ and this is not a Sunday, then the last week is lasting less than 7 days and the corresponding value in the output is marked with an asterisk 
-	Start_date (str):  variable name of the -dataset- containing the date when the information recorded in record starts to be valid
-	End_date (str):  variable name of the -dataset- containing the date when the information recorded in the record ends to be valid; if this is missing, end_study_time is assumed to be the end of the record; end_record must be a date occurring after start_date; the same person cannot have overlapping intervals  start_date- end_date;
-	Birth_date (str): (optional) variable name containing the birth date of the person; if this s specified, the option agebands is required
-	Outcomes (list of strings): list of variable names, each associated to an event; each variable contains the first date when an event occurred to a person; the value must be the same across all the records of the same person
-	Strata (list of strings): (optional) list of variable names, each associated to a categorical variable; 
-	Name_event (string): name of the variable of dataset_events containing  the label of the events to be observed
-	Date_event (string): name of the variable of dataset_events containing  the date of the events to be observed
-	Age_bands (list of integers): (optional, but required if birth_date is specified) list of integers representing the left limit of age bands: eg c(0,20,40,80) means 0-19, 20-29,40-79,80+(the last is missing if include_remaning_ages=FALSE)
-	Unit_of_age (list of strings ’days’, ‘months’ ,’years’): (optional) list of as many strings as the agebands list. It specifies whether the corresponding ageband is expressed in days, months, or years. If this parameter is not specified, ‘years’ is assumed for all agebands
-	Increment (year/month/week/day): across which timeframe rates need to be computed from start_study_time to end_study_time
-	include_remaning_ages (bool): (optional) default TRUE: if TRUE, an additional ageband is created, containing all ages after the last one 
-	Aggregate (bool): (optional) default TRUE: if TRUE, the persontime of all events and overall, as well as the number of events, are aggregated per timeframe and, if specified, by agebands and/or strata


# Structure of output
*	If aggregate is FALSE:
 *	person identifier (same name as in the input)
 *	timeframe; if timeframe was ‘yearly’ this, this contains the list of calendar years from start_study_time to end_study_time; if timeframe was ‘monthly’ this, this contains the list of calendar months from start_study_time to end_study_time; if timeframe was ‘weekly’ this, this contains the list of calendar weeks from start_study_time to end_study_time; if timeframe was ‘daily’ this, this contains the list of days from start_study_time to end_study_time
 *	strata (if specified): contains the values of the strata observed by the person in the timeframes
 *	ageband (if specified): contains the values of the agebands observed by the person in the timeframes
 *	persontime: contains the overall person time of the person in that timeframe in those strata/agebands; unit of persontime is always days
 *	persontime for each outcome: contains the person time of the person in that timeframe in those strata/agebands, considering that outcome as a censoring criterion; 
 *	a binary variable for each outcome, named as the corresponding outcome, containing 1 if the person experienced the outcome at the end of that timeframe

* If aggregate is TRUE:
  * The same as before, but persontime variables are aggregated per timeframe and, if specified, per ageband and/or strata


