

#tmpname2 <- tempfile(pattern = "periods", tmpdir = tempdir(), fileext = ".rds")
#saveRDS(Dataset, tmpname2)
#if(exists("tmpname2")) if(file.exists(tmpname2))  unlink(tmpname2)
#Situation where results are be aggregated and all rec_periods are 0 
####

if(Aggregate == T & exists("tmpname") & (sum(Rec_period == 0) == length(Rec_period))){
  
  if(print)print("Calculate aggregated resuts for recurrent events if only rec_periods of 0")   
  
  Dataset <- Dataset[, lapply(.SD, sum), .SDcols = "Persontime", by = by_colls]
  lapply(by_colls, function(x) Dataset[, eval(x) := as.character(get(x))])
  
  
  Events <- CalculateNumeratorAggregated(
    
    Dataset = readRDS(tmpname2)[, c(Person_id, Start_date, End_date, Birth_date, Strata), with = F],
    Person_id = Person_id,
    Start_date = Start_date, 
    End_date = End_date, 
    Dataset_events = readRDS(tmpname), 
    Agebands.file = Agebands_list, 
    Times.file = Dummy, 
    Name_event = Name_event, 
    Date_event = Date_event, 
    Birth_date = Birth_date, 
    Strata = by_colls
    
  )
  
  Dataset <- merge(x = Dataset, y = Events, by = by_colls, all.x = T )   
  Dataset[is.na(Dataset), ] <- 0
  lapply(Outcomes_rec[Rec_period == 0], function(x) Dataset[, eval(paste0("Persontime_", x)) := Persontime])
  
}


####

if((sum(Rec_period == 0) != length(Rec_period)) & Aggregate & exists("tmpname")){
  
  tmpname3 <- tempfile(pattern = "persontime", tmpdir = tempdir(), fileext = ".rds")
  saveRDS(Dataset[, lapply(.SD, sum), .SDcols = "Persontime", by = by_colls], tmpname3)
  
  
}
