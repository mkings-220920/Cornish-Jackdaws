#Calculate the number of group members with which an individual has had multiple associations. Output is a time-varying covariate.

repeat_degree<-function(df){
  repeat_success_deg<-c()
  repeat_fail_deg<-c()
  for(i in 1:nrow(df)){   #Loop through each record in dataset (records ordered chronologically)
    if(i<3){                 
      repeat_success_deg<-c(repeat_success_deg,0)
      repeat_fail_deg<-c(repeat_fail_deg,0)
    } else {
      if(df[i,"TYPE"]!="NON-EVENT"){
        tempdf<-df[1:(i-1),]    #all previous events and 'non-events' up to and including previous event
        } else {
          tempdf<-df[1:(i-2),]   #get previous 'non-event'
          }
      repeat_dyads_success<-tempdf[which((as.character(tempdf$TARGET)==toString(df[i,"TARGET"]) | as.character(tempdf$SOURCE)==toString(df[i,"TARGET"])) & tempdf$TYPE=="COOP_SUCCESS" & tempdf$dyad.has.previous.success>0),"DYAD"]  #find previous event records ('non-events' disregarded) which contain the 'TARGET' from the current event, that were successful events and not a pairing's first event. Dyad labels from each of the records stored. 
      repeat_dyads_fail<-tempdf[which((as.character(tempdf$TARGET)==toString(df[i,"TARGET"]) | as.character(tempdf$SOURCE)==toString(df[i,"TARGET"])) & tempdf$TYPE=="COOP_FAIL" & tempdf$dyad.has.previous.fail>0),"DYAD"]  #find dyad previous event records ('non-events' disregarded) that contain the 'TARGET' from the current event, that were unsuccessful events and not a pairing's first event. Dyad labels from each of the records stored. 
      repeat_success_deg<-c(repeat_success_deg,length(unique(repeat_dyads_success)))  #Calculate and store number of successful dyads that the current 'TARGET' has been part of
      repeat_fail_deg<-c(repeat_fail_deg,length(unique(repeat_dyads_fail)))    #Calculate and store number of successful dyads that the current 'TARGET' has been part of
    }
  }
  df$repeat_success_deg<-repeat_success_deg
  df$repeat_fail_deg<-repeat_fail_deg
  return(df)
}

