#MK concatenate SOLO events (only with other SOLO events)

concat_onlysolo<-function(x){
  #Filter to get a dataframe of only SOLO events
  solo_df<-x[x$TYPE=="SOLO_NO_COOP",]
  solo_df<-solo_df[order(as.numeric(solo_df[,"TIME"])),]
  nonsolo_df<-x[x$TYPE!="SOLO_NO_COOP",]
  
  #Find candidate entries for concatenation
  sdiff<-diff(as.numeric(solo_df[,"TIME"]))      #Get the time difference (seconds) between all SOLO events
  concat_locs<-which(sdiff<15 & solo_df[1:(nrow(solo_df)-1),"SOURCE"]==solo_df[2:nrow(solo_df),"SOURCE"])    #Find neighbouring SOLO events by the same individual that are separated by less than 15 seconds
  
  if(length(concat_locs)){       #If there are SOLO events to be concatenated
    di<-diff(concat_locs)             #Determine the number of events between successive SOLO events by the same individual
  
    #Find start and end indices for concatenating solo events
    s_inds<-c(concat_locs[1])                       #The first SOLO event to be concatenated has a fixed start time (cannot be a continuation of a previous event)
    for(i in 1:length(di)){                      #For every difference (in number of events) between SOLO events to be evaluated for concatenation
      if(i!=1){                                    #If the current event is not in the first pair of events to evaluate
        if(di[i-1]>1){                                #If there were other (i.e. non-solo) events that occurred between the previous SOLO event (by this same individual) and the current solo event...
          s_inds<-c(s_inds,concat_locs[i])             #then this current solo event cannot be a continuation of the previous event (by this individual), so the current event is deemed a start point.
        }
      }
    }
  
    e_inds<-concat_locs[length(concat_locs)]+1           #The last SOLO event to be concatenated has a fixed end time (must terminate the previous event if the current event is a continuation of the previous one)
    for(i in 1:length(di)){                                 #For every difference (in number of events) between SOLO events to be evaluated for concatenation
      if(i!=length(di)){                                       #If the current event is not in the final pair of events to evaluate
        if(di[i]>1){                                             #If there were other (i.e. non-solo) events that occurred between the current SOLO event and the next SOLO event (by this same individual)...
          e_inds<-c(e_inds,(concat_locs[i]+1))                    #then the next solo event (by this individual) cannot be a continuation of this event, so the current event is deemed an end point.
        }
      }
    }
  
    #create new solo_df using start and end points (from above)
    s_inds<-s_inds[order(s_inds)]                             #ensure that start points (event indices) are ordered in ascending fashion
    e_inds<-e_inds[order(e_inds)]                             #ensure that end points (event indices) are ordered in ascending fashion
    sololist<-Map(function(a,b) data.frame(SOURCE=solo_df[a,"SOURCE"],TARGET=solo_df[a,"TARGET"],TIME=solo_df[a,"TIME"],TYPE=solo_df[a,"TYPE"],WEIGHT=(as.numeric(solo_df[b,"TIME"])+as.numeric(solo_df[b,"WEIGHT"]))-as.numeric(solo_df[a,"TIME"]),SITE=solo_df[a,"SITE"],lock_bool=solo_df[a,"lock_bool"],high_bool=solo_df[a,"high_bool"],low_bool=solo_df[a,"low_bool"],ANTENNA=solo_df[a,"ANTENNA"]),a=s_inds,b=e_inds)   #For each pair of start and end points, create a dataframe in Eventnet format containing information regarding the concatenated SOLO event 
    solodf2<-do.call(rbind,sololist)              #Get a single dataframe of all post-concatenation solo events
    out_df<-rbind(solodf2,nonsolo_df)             #Re-combine solo and non-solo events into a single dataset
    return(out_df)
  } else {
    return(x)
  }
}






