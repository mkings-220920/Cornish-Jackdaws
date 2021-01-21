#MK recursively concatenate PRE/POST coop events with neighbouring SOLO events (if they are performed by the same individual within a 15 second time threshold)

concat_soloprepost<-function(x){
  
  #split REM dataframe in two (into solo and non-solo events dataframes)
  solo_df<-x[x[,"TYPE"]=="SOLO_NO_COOP",]
  nonsolo_df<-x[x[,"TYPE"]!="SOLO_NO_COOP",]
  
  #locate pre and post coop events
  pre_inds<-which(nonsolo_df[,"TYPE"]=="SOLO_PRE_COOP")
  post_inds<-which(nonsolo_df[,"TYPE"]=="SOLO_POST_COOP")
  
  #concatenate pre-coop and solo events
  rmvls_precoop<-c()
  for(i in 1:length(pre_inds)){            #for each PRE-COOP event 
    ttt<-as.numeric(nonsolo_df[i,"TIME"])-as.numeric(solo_df[,"TIME"])          #Get all time differences (seconds) between current non-solo event and all solo events
    nearest<-which(ttt>0 & ttt<15)[which.min(ttt[ttt>0 & ttt<15])]              #Identify index of closest preceeding solo event to the current non-solo event (must be within 15 seconds - the functional resolution of the task)                          
    if(length(nearest) && (as.numeric(nonsolo_df[i,"TIME"])-as.numeric(solo_df[nearest,"TIME"]))<15 && nonsolo_df[i,"SOURCE"]==solo_df[nearest,"SOURCE"]){    #If a nearby preceeding solo event exists, within the 15 second threshold, and the RFID code of the individual is the same for the SOLO event as for the PRE-COOP event  
      rmvls_precoop<-c(rmvls_precoop,nearest)                 #Mark the nearby solo event for removal
      nonsolo_df$WEIGHT[i]<-as.numeric(nonsolo_df[i,"WEIGHT"]) + (as.numeric(nonsolo_df[i,"TIME"])-as.numeric(solo_df[nearest,"TIME"]))     #Update the duration of the PRE_COOP event (add the estimated duration of the solo event to the duration of the PRE_COOP event to get the updated duration)
    }
  }
  
  #concatenate post-coop and solo events (as for the above block, except for POST-COOP)
  rmvls_postcoop<-c()
  for(i in 1:length(post_inds)){             #for each POST-COOP event 
    ttt<-as.numeric(solo_df[,"TIME"])-as.numeric(nonsolo_df[i,"TIME"])     #Get all time differences (seconds) between current non-solo event and all solo events
    nearest<-which(ttt>0 & ttt<15)[which.min(ttt[ttt>0 & ttt<15])]            #Identify index of closest succeeding solo event to the current non-solo event (must be within 15 seconds - the functional resolution of the task)                          
    if(length(nearest) && (as.numeric(solo_df[nearest,"TIME"])-as.numeric(nonsolo_df[i,"TIME"]))<15 && nonsolo_df[i,"SOURCE"]==solo_df[nearest,"SOURCE"]){       #If a nearby succeeding solo event exists, within the 15 second threshold, and the RFID code of the individual is the same for the SOLO event as for the POST-COOP event  
      rmvls_postcoop<-c(rmvls_postcoop,nearest)           #Mark the nearby solo event for removal
      nonsolo_df$WEIGHT[i]<-as.numeric(nonsolo_df[i,"WEIGHT"])+(as.numeric(solo_df[nearest,"TIME"])-as.numeric(nonsolo_df[i,"TIME"]))     #Update the duration of the PRE_COOP event (add the estimated duration of the solo event to the duration of the POST_COOP event to get the updated duration)
    }
  }
  
  #perform removals and re-form dataframe
  all_rmvls<-c(rmvls_precoop,rmvls_postcoop)                        #Get indices of all events to be removed
  new_solo<-solo_df[(!1:nrow(solo_df) %in% all_rmvls),]              #Remove solo events that have been concatenated with PRE or POST coop
  mod_df<-rbind(new_solo,nonsolo_df)                                 #Re-combine (clipped) solo dataframe with the (edited PRE and POST COOP event durations) dataframe of non-solo events
  
  #Recursion
  if(length(all_rmvls)){                #If the current function call identified events to be concatenated
    concat_soloprepost(mod_df)            #Call the function again with the (new) edited dataframe
  } else {
    return(mod_df)                    #Keep calling the function until there are no more events to concatenate
  }
}














