


function(
  
  Dataset,
  Start_date,
  End_date,
  Dataset_events,
  Person_id,
  Name_event,
  Outcomes_rec,
  Rec_period, 
  Aggregate = F,
  Strata
  
  
  
  
){}


#Standardize names because data.table gives problems in some occasions when working with varaibles as inputs
setnames(Dataset_events, c(eval(Person_id), eval(Name_event), eval(Date_event)), c("ID","EVNT", "DTEVNT"))
setnames(Dataset, c(eval(Person_id), eval(Start_date), eval(End_date)), c("ID", "ST", "EN"))

#Select events of interest based on start and end dates also called persontime
if(!Aggregate) Dataset_temp <- copy(Dataset)[ID %in% unique(Dataset_events[["ID"]]),][, c("ID", "ST", "EN"), with = F]
if(Aggregate) Dataset_temp <- copy(Dataset)[ID %in% unique(Dataset_events[["ID"]]),][, c("ID", "ST", "EN", Strata), with = F]

Dataset_events <- Dataset_events[, dummy := DTEVNT]
setkeyv(Dataset_temp, c("ID", "ST", "EN"))
Dataset_events_count <- foverlaps(Dataset_events, Dataset_temp, by.x = c("ID","DTEVNT","dummy"), nomatch = 0L, type = "any")





Outcomes_rec1 <-  Outcomes_rec[ Outcomes_rec  %in% Outcomes_rec[!Rec_period == 0]]  
Outcomes_rec0 <-  Outcomes_rec[! Outcomes_rec %in% Outcomes_rec1] 

#Dataset_events_rec0  <- copy(Dataset_events)[EVNT %in% Outcomes_rec0]
Dataset_events_rec1  <- copy(Dataset_events)[EVNT %in% Outcomes_rec1]

Rec_period1 <- Rec_period[Outcomes_rec %in% Outcomes_rec1]

#rm(Dataset_events)
gc()

#Calculate for every observation period the amount of persontime that needs to subtracted for every event
################################################################################################################################


if(nrow(Dataset_events_rec1) > 0){
  
  print("Calculate start and end dates for the censoring periods")
  
  for(i in 1:length(Rec_period1)){
    
    print(paste("Set censoring start and end dates for outcome ",Outcomes_rec1[i], " with a duration of ",Rec_period1[i]," days"))
    #Dataset_events_rec1 <- Dataset_events_rec1[dif != 0 & get(Name_event) == Outcomes_rec1[i] & dif < Rec_period1[i], Delete := T ][is.na(Delete),]
    Dataset_events_rec1 <- Dataset_events_rec1[EVNT == Outcomes_rec1[i], ":=" (RecSTDT = DTEVNT, RecENDT = DTEVNT + Rec_period1[i])]  
    gc()
    
  }
  
  print("Calculate days to subtract from persontime for recurrent events")
  
  #if(!Aggregate) Dataset_temp <- copy(Dataset)[ID %in% unique(Dataset_events_rec1[["ID"]]),][, c("ID", "ST", "EN"), with = F]
  #if(Aggregate) Dataset_temp <- copy(Dataset)[ID %in% unique(Dataset_events_rec1[["ID"]]),][, c("ID", "ST", "EN", Strata), with = F]
  
  setkeyv(Dataset_temp, c("ID", "ST", "EN"))
  Dataset_events_rec2 <- foverlaps(Dataset_events_rec1, Dataset_temp, by.x = c("ID","RecSTDT","RecENDT"), nomatch = 0L, type = "any")
  
  if(nrow(Dataset_events_rec2) > 0){
    
    Dataset_events_rec2 <- Dataset_events_rec2[, row2 := row.names(Dataset_events_rec2)]
    Dataset_events_rec2 <- Dataset_events_rec2[,':=' (start_date2 = max(ST, RecSTDT, na.rm = T), end_date2 = min(EN + 1, RecENDT, na.rm = T)), keyby = row2]     
    Dataset_events_rec2 <- Dataset_events_rec2[, SUBTR := (as.numeric(end_date2) - as.numeric(start_date2)),by = row2]
    
    Dataset_events_rec2 <- Dataset_events_rec2[, var := paste0("SUBTRCUM_", EVNT)]
    
    if(Aggregate) Dataset2 <- data.table::dcast(Dataset_events_rec2, formula(paste0(paste(Strata, collapse = " + "), " ~  var")) , value.var = "SUBTR", fun.aggregate = sum)
    
    if(!Aggregate){
      Dataset_events_rec2 <- data.table::dcast(Dataset_events_rec2, ID + ST + EN ~  var, value.var = "SUBTR", fun.aggregate = sum)
      Dataset <- merge(x = Dataset, y = Dataset_events_rec2 , by = c("ID", "ST", "EN"), all.x = T , all.y = F, allow.cartesian = F)
    }
    
  }else{
    
    print("All recurrent events fall witout the period of interest")
  }
  
  rm(Dataset_temp)  
  gc()
}


rm(Dataset_events_rec1, Dataset_events_rec2)
gc()

if(Aggregate){
  Dataset <- merge(Dataset1, Dataset2, all = T, by = Strata)
  rm(Dataset1, Dataset2)
  gc()    
}


