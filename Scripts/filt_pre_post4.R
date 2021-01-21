#MK filter PRE and POST cooperation solo events (remove duplicates)

filt_pre_post4<-function(x){
  pre_locs<-which(x[,"TYPE"]=="PRE_COOP")                   #Get indices of all PRE-COOP events (SOLO events that lead into a COOP event)
  post_locs<-which(x[,"TYPE"]=="POST_COOP")                #Get indices of all POST-COOP events (SOLO events that lead on from a COOP event)
  pre_remove<-sapply(pre_locs,function(a) if((a-1)>0)  {if(x[(a-1),"TYPE"] %in% c("SOLO_NO_COOP","SCROUNGE","REPLACEMENT","POST_COOP")){NULL} else {a}})  #For each PRE_COOP event, check that it is not the first event in the datastream, if not then if the event before a PRE_COOP was a SOLO visit then keep, otherwise remove as they might be duplicates
  post_remove<-sapply(post_locs,function(a) if((a+1)<=nrow(x)) {if(x[(a+1),"TYPE"] %in% c("SOLO_NO_COOP","SCROUNGE","REPLACEMENT","PRE_COOP")){NULL} else {a}})    #For each PRE_COOP event, check that it is not the last event in the datastream, if not then if the event after a POST_COOP was a SOLO visit then keep, otherwise remove as they might be duplicates
  all_remove<-c(pre_remove,post_remove)                  #Get all indices of PRE and POST COOP labels to remove.
  outREM<-x[which(!1:nrow(x) %in% all_remove),]           #Remove duplicate PRE and POST COOP labels
  return(outREM)
}

