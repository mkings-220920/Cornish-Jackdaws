#MK Get cooperation events and Post-Cooperation events in Eventnet format

coop_postcoop2<-function(x){
  
  #Get successful cooperation events in Eventnet format, calculate Post-Cooperation latencies (time between first and second departures of two individuals that have successfully cooperated) for successful cooperation events (and get in REM format)
  success_locs<-which(x[,2]=="STARTCOOP_SUCCESS")     #Find the indices of all of the successful cooperation events in the dataset
  if(length(success_locs)){
    #Find departure labels for each individual involved in each successful cooperation event
    idlist<-sapply(success_locs,function(a) strsplit(toString(x[a,3]),"_"))  #Get a list with two entries for each element, such that each element contains the IDs of both individuals that participated in a successful cooperation event. List length equal to total number of successful cooperation events.
    dt1<-Map(function(a,b) as.numeric(x[(x[,3]==toString(unlist(b)[1])) & (x[,2] %in% c("TAG_DEPARTED_A","TAG_DEPARTED_B","ATAG_DEPARTED_A","ATAG_DEPARTED_B")),"secs"])-as.numeric(x[a,"secs"]),a=success_locs,b=idlist)  #For each SOURCE (first to arrive) in a successful cooperation (COOP_SUCCESS) event, get times of all of its departures in relation to the time of onset of the coop event
    dt1<-lapply(dt1,function(a) min(a[which(a>=0)]))    #for each of the list entries generated above get the minimum latency (that's greater than zero) to get the departure label for each SOURCE that follows each cooperation event
    dt2<-Map(function(a,b) as.numeric(x[(x[,3]==toString(unlist(b)[2])) & (x[,2] %in% c("TAG_DEPARTED_A","TAG_DEPARTED_B","ATAG_DEPARTED_A","ATAG_DEPARTED_B")),"secs"])-as.numeric(x[a,"secs"]),a=success_locs,b=idlist)   #For each TARGET (second to arrive) in a successful cooperation (COOP_SUCCESS) event, get times of all of its departures in relation to the time of onset of the coop event
    dt2<-lapply(dt2,function(a) min(a[which(a>=0)]))    #for each of the list entries generated above get the minimum latency (that's greater than zero) to get the departure label for each TARGET that follows each cooperation event

    #Output successful cooperation events in Eventnet format. Note, WEIGHT is time from coop event onset to time of first departure.
    coop_success<-Map(function(a,b,d,e) data.frame(SOURCE=unlist(d)[1],TARGET=unlist(d)[2],TIME=as.numeric(x[e,"secs"]),TYPE="COOP_SUCCESS",WEIGHT=min(c(a,b)),SITE=x[e,"SITE"],ANTENNA=NA),a=dt1,b=dt2,d=idlist,e=success_locs)
    csdf<-do.call(rbind,coop_success)
    
    #Output post-coop success events (solo feeding following cessation of a successful cooperation event). Note, WEIGHT is difference in time between the time of the start of a coop event to the first departure and the time from start of coop event to the second departure (Difference gives latency between first and second departures)
    post_coopsuccess<-Map(function(a,b,d,e) if(a<b) {data.frame(SOURCE=unlist(d)[2],TARGET=unlist(d)[1],TIME=(as.numeric(x[e,"secs"])+a),TYPE="POST_COOP",WEIGHT=(b-a),SITE=x[which(x[,"secs"]==(as.numeric(x[e,"secs"])+a))[1],"SITE"],ANTENNA=NA)} else if(b<a) {data.frame(SOURCE=unlist(d)[1],TARGET=unlist(d)[2],TIME=(as.numeric(x[e,"secs"])+b),TYPE="POST_COOP",WEIGHT=(a-b),SITE=x[which(x[,"secs"]==(as.numeric(x[e,"secs"])+b))[1],"SITE"],ANTENNA=NA)} else {NULL},a=dt1,b=dt2,d=idlist,e=success_locs)
    pcsdf<-do.call(rbind,post_coopsuccess)  
  } else {
    csdf<-NULL
    pcsdf<-NULL
  }  
  
  #The block below is as for the success block above, except for unsuccessful cooperation events
  fail_locs<-which(x[,2]=="STARTCOOP_FAIL")
  if(length(fail_locs)){
    fail_idlist<-sapply(fail_locs,function(a) strsplit(toString(x[a,3]),"_"))
    faildt1<-Map(function(a,b) as.numeric(x[(x[,3]==toString(unlist(b)[1])) & (x[,2] %in% c("TAG_DEPARTED_A","TAG_DEPARTED_B","ATAG_DEPARTED_A","ATAG_DEPARTED_B")),"secs"])-as.numeric(x[a,"secs"]),a=fail_locs,b=fail_idlist)
    faildt1<-lapply(faildt1,function(a) min(a[which(a>=0)]))
    faildt2<-Map(function(a,b) as.numeric(x[(x[,3]==toString(unlist(b)[2])) & (x[,2] %in% c("TAG_DEPARTED_A","TAG_DEPARTED_B","ATAG_DEPARTED_A","ATAG_DEPARTED_B")),"secs"])-as.numeric(x[a,"secs"]),a=fail_locs,b=fail_idlist)
    faildt2<-lapply(faildt2,function(a) min(a[which(a>=0)]))
    
    coop_fail<-Map(function(a,b,d,e) data.frame(SOURCE=unlist(d)[1],TARGET=unlist(d)[2],TIME=as.numeric(x[e,"secs"]),TYPE="COOP_FAIL",WEIGHT=min(c(a,b)),SITE=x[e,"SITE"],ANTENNA=NA),a=faildt1,b=faildt2,d=fail_idlist,e=fail_locs)
    cfdf<-do.call(rbind,coop_fail)
    
    post_coopfail<-Map(function(a,b,d,e) if(a<b) {data.frame(SOURCE=unlist(d)[2],TARGET=unlist(d)[1],TIME=(as.numeric(x[e,"secs"])+a),TYPE="POST_COOP",WEIGHT=(b-a),SITE=x[which(x[,"secs"]==(as.numeric(x[e,"secs"])+a))[1],"SITE"],ANTENNA=NA)} else if(b<a) {data.frame(SOURCE=unlist(d)[1],TARGET=unlist(d)[2],TIME=(as.numeric(x[e,"secs"])+b),TYPE="POST_COOP",WEIGHT=(a-b),SITE=x[which(x[,"secs"]==(as.numeric(x[e,"secs"])+b))[1],"SITE"],ANTENNA=NA)} else {NULL},a=faildt1,b=faildt2,d=fail_idlist,e=fail_locs)
    pcfdf<-do.call(rbind,post_coopfail)  
  } else {
    cfdf<-NULL
    pcfdf<-NULL
  }
  
  return(list(COOP_SUCCESS=csdf,COOP_FAIL=cfdf,POSTCOOP_SUCCESS=pcsdf,POSTCOOP_FAIL=pcfdf))
  
}











