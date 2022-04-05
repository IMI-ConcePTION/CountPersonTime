


CalculateNumeratorNotRecurrent <- function(Dataset_events, Dataset, Person_id, , , Start_date,
                            End_date,  =Strata NULL,Outcomes_nrec ,
                            Name_event , Date_event ,  Aggregate = T,
                            print = F, ){




setorderv(Dataset_events,c(Person_id,Name_event,Date_event))
Dataset_events[,Recurrent := cumsum(!is.na(get(Date_event))),by=c(Person_id,Name_event)]

if(nrow(Dataset_events) > 0){ 
  
  if(print) print("If Rec_events = F then selecting only the first event")
  Dataset_events <- Dataset_events[Recurrent==1,]
  Dataset_events <- dcast(Dataset_event, get(Person_id) + Recurrent ~ get(Name_event), value.var = eval(Date_event))
  setcolorder(Dataset_events,neworder = c('Person_id','Recurrent',Outcomes_nrec))
  
  setkeyv(Dataset,Person_id)
  setkey(Dataset_events,Person_id)
  Dataset <- Dataset_events[Dataset,][,Recurrent := NULL]
  setnames(Dataset, "Person_id", eval(Person_id))
  
}
rm(Dataset_events)
gc()

}

lapply(Outcomes_nrec, function(x) if (!x %in% colnames(Dataset)) Dataset <- Dataset[, eval(x) := as.IDate(NA, format = "%d%m%Y")]) 

  invisible(lapply(Outcomes, function(x) Dataset <- Dataset[, eval(x) := as.IDate(NA, format = "%d%m%Y")]))
  Dataset_events_nrec <- copy(Dataset_events)
  Dataset_events_rec <- copy(Dataset_events)
  Dataset_events_rec3 <- copy(Dataset_events)




if(nRec_events){
  lapply(Outcomes_nrec,function(x)Dataset[,paste0(eval(x),"_b") := fifelse(!is.na(get(x)) & get(x) %between% list(get(Start_date),get(End_date)),1,0)])
  lapply(Outcomes_nrec, function(x) Dataset[,paste0("Persontime_",x) := fifelse(!is.na(get(x)) & get(x) < get(Start_date), 0, Persontime)])
  #lapply(Outcomes,function(x)Dataset[get(x) %between% list(get(Start_date),get(End_date)),`:=`(paste0(eval(x),"_b")= 1, paste0("Persontime_",x) = .(get(x)-get(Start_date)+1))])
  lapply(Outcomes_nrec,function(x)Dataset[get(x) %between% list(get(Start_date),get(End_date)),paste0("Persontime_",x) := .(get(x)-get(Start_date)+1)])
}



