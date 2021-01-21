#MK concatenate neighbouring cooperation events (to remove duplicates)

concat5<-function(x){   
  #Concatenate duplicate STARTCOOP_SUCCESS labels (and remove interstitial labels)
  success_locs<-which(x[,2] %in% c("STARTCOOP_SUCCESS"))       #Identify indices of successful cooperation events
  s_rmvls<-c()
  s_rmvls_pos<-c()
  if(length(success_locs)){                          #If there are successful cooperation events in the datastream
    for(i in 1:length(success_locs)){               #For every successful cooperation events
      if(i!=length(success_locs)){                    #If the current successful cooperation event is not the final cooperation event in the datastream
        frst_dyad<-toString(x[success_locs[i],3])             #dyad IDs (combined in one string) of ith COOP_SUCCESS
        scnd_dyad<-toString(x[success_locs[i+1],3])             #dyad IDs (combined in one string) of i+1th COOP_SUCCESS
        temp_spl<-unlist(strsplit(x[success_locs[i],3],"_"))             #Get ith COOP_SUCCESS IDs in list format. Each list elements contains two elements (SOURCE ID and TARGET ID)
        rev_dyad<-toString(paste(temp_spl[2],temp_spl[1],sep="_"))          #ith dyad as a single string in the reverse configuration (TARGET then SOURCE) 
        s_t_diff<-(as.numeric(x[success_locs[i+1],"secs"])-as.numeric(x[success_locs[i],"secs"]))<15    #are ith and i+1th COOP_SUCCESS events separated by less than 15 seconds (TRUE/FALSE)?
        if((frst_dyad==scnd_dyad | scnd_dyad==rev_dyad) & s_t_diff){              #if ith and i+1th COOP_SUCCESS events separated by less than 15 seconds and both events contain the same individuals (irrespective of who is the SOURCE or the TARGET)
          all_ids<-unique(x[success_locs[i]:success_locs[i+1],3])                 #Get IDs of all individuals and dyads in events between and including the ith and i+1th COOP_SUCCESS events
          if(!length(which(!all_ids %in% c(temp_spl[1],temp_spl[2],frst_dyad,scnd_dyad,rev_dyad)))){         #If there aren't any other individuals participating in the events identified between the ith and i+1th COOP_SUCCESS events
            s_rmvls<-c(s_rmvls,(x[(success_locs[i]),"secs"]):(x[(success_locs[i+1]),"secs"]))               #Note the times of the events between the ith and i+1th COOP_SUCCESS events for removal
            s_rmvls_pos<-c(s_rmvls_pos,(x[(success_locs[i]),"pos"]+1):(x[(success_locs[i+1]),"pos"]))       #Note the indices of the events between the ith and i+1th COOP_SUCCESS events for removal
          }
        }
      }
    }
  }
  
  #Concatenate duplicate STARTCOOP_FAIL labels (and remove interstitial labels). As for the above STARTCOOP_SUCCESS block, except for STARTCOOP_FAIL events
  fail_locs<-which(x[,2] %in% c("STARTCOOP_FAIL"))
  f_rmvls<-c()
  f_rmvls_pos<-c()
  if(length(fail_locs)){
    for(i in 1:length(fail_locs)){
      if(i!=length(fail_locs)){
        frst_dyad<-x[fail_locs[i],3]
        scnd_dyad<-x[fail_locs[i+1],3] 
        temp_spl<-unlist(strsplit(x[fail_locs[i],3],"_"))
        rev_dyad<-toString(paste(temp_spl[2],temp_spl[1],sep="_"))
        if((frst_dyad==scnd_dyad || scnd_dyad==rev_dyad) && (as.numeric(x[fail_locs[i+1],"secs"])-as.numeric(x[fail_locs[i],"secs"]))<15){
          all_ids<-unique(x[fail_locs[i]:fail_locs[i+1],3])
          if(!length(which(!all_ids %in% c(temp_spl[1],temp_spl[2],frst_dyad,scnd_dyad,rev_dyad)))){
            f_rmvls<-c(f_rmvls,(x[(fail_locs[i]),"secs"]):(x[(fail_locs[i+1]),"secs"]))
            f_rmvls_pos<-c(f_rmvls_pos,(x[(fail_locs[i]),"pos"]+1):(x[(fail_locs[i+1]),"pos"]))
          }
        }
      }
    }
  }
  
  all_rmvls<-c(s_rmvls,f_rmvls)     #Get all of the times of events identified for removal 
  all_rmvls_pos<-c(s_rmvls_pos,f_rmvls_pos)         #Get all of the indices of events identified for removal
  if(length(all_rmvls)){                             #If there are events to be removed from the datastream
    rmvls_ind<-which((x[,"secs"] %in% all_rmvls) & ((x[,"pos"] %in% all_rmvls_pos) | x[,"pos"]==1))       #Identify events for which the event index and the time of onset of the event are marked for removal. For events that have been added to the datastream in post-processing (missing DEPARTURE labels and REPLACEMENTS), only time is used for removal purposes (Index is set to 1 for these events to preserve event order in the datastream in cases where multiple events have occurred within a second of each other)    
    outdf<-x[which(!1:nrow(x) %in% rmvls_ind),]           #Remove the events made redundant by concatenation of COOP events from the datastream 
    return(outdf)
  } else {
    return(x)
  }
  
} 






















