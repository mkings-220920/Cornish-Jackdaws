#MK find scrounging events (where a scrounging event is a solo event whilst the HIGH quality doors are open by an individual that did not participate in the successful cooperation event that brought about the HIGH open door state)

get_scrounge2<-function(x){                     
  cooptemp<-x[x[,"TYPE"]=="COOP_SUCCESS",]        #Get a dataframe just containing the successful cooperation events
  labelstore<-c()                                #For storage of event TYPE labels
  for(i in 1:nrow(x)){        #for each event in the original datastream
    if(x[i,"high_bool"]==1 & x[i,"TYPE"]=="SOLO_NO_COOP"){         #if the current event is a solo event that occurs during a period in which the HIGH quality doors are open.
      coop_rel_time<-as.numeric(x[i,"TIME"])-as.numeric(cooptemp[,"TIME"])    #Find the time difference from the onset of the potential scrounge event to the onset of (all) successful coop events
      cooploc<-which(coop_rel_time>0 & coop_rel_time<15)   #Identify any successful coop events within 15 seconds prior to the potential scrounge event
      if(length(cooploc)){          #if there is a successful coop event within 15 seconds of potential scrounge event
        cooploc<-cooploc[length(cooploc)]  #only deal with most recent coop event (account for if there are multiple coop events that are relevant) 
        label<-if(x[i,"SOURCE"]==cooptemp[cooploc,"SOURCE"] | x[i,"SOURCE"]==cooptemp[cooploc,"TARGET"]){"SOLO_NO_COOP"} else {"SCROUNGE"}  #if the potential scrounger wasn't a member of the dyad that triggered the relevant COOP_SUCCESS event then label it a SCROUNGE
        labelstore<-c(labelstore,label)              #Update the current event TYPE label (from SOLO_NO_COOP to SCROUNGE)
      } else {
        labelstore<-c(labelstore,toString(x[i,"TYPE"]))     #Keep the current event TYPE label the same
      }
    } else {
      labelstore<-c(labelstore,toString(x[i,"TYPE"]))       #Keep the current event TYPE label the same
    }
  }
  x$TYPE<-labelstore                      #Replace the event TYPE column in the dataframe
  return(x)
}
