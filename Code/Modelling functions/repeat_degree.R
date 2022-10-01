
repeat_degree<-function(df){
  repeat_success_deg<-c()
  repeat_fail_deg<-c()
  for(i in 1:nrow(df)){
    if(i<3){
      repeat_success_deg<-c(repeat_success_deg,0)
      repeat_fail_deg<-c(repeat_fail_deg,0)
    } else {
      if(df[i,"TYPE"]!="NON-EVENT"){
        tempdf<-df[1:(i-1),]
        } else {
          tempdf<-df[1:(i-2),]
          }
      repeat_dyads_success<-tempdf[which((as.character(tempdf$TARGET)==toString(df[i,"TARGET"]) | as.character(tempdf$SOURCE)==toString(df[i,"TARGET"])) & tempdf$TYPE=="COOP_SUCCESS" & tempdf$dyad.has.previous.success>0),"DYAD"]
      repeat_dyads_fail<-tempdf[which((as.character(tempdf$TARGET)==toString(df[i,"TARGET"]) | as.character(tempdf$SOURCE)==toString(df[i,"TARGET"])) & tempdf$TYPE=="COOP_FAIL" & tempdf$dyad.has.previous.fail>0),"DYAD"]
      repeat_success_deg<-c(repeat_success_deg,length(unique(repeat_dyads_success)))
      repeat_fail_deg<-c(repeat_fail_deg,length(unique(repeat_dyads_fail)))
    }
  }
  df$repeat_success_deg<-repeat_success_deg
  df$repeat_fail_deg<-repeat_fail_deg
  return(df)
}

