#MK find dyad changes (label whether each cooperation event contains the same, or different, two individuals than the last cooperation event)

dyad_change1<-function(x){
  prev_dyadfor<-c()
  prev_dyadrev<-c()
  dyvec<-c()
  for(i in 1:nrow(x)){                                             #Loop through every event in the dataset
    if(toString(x[i,"SOURCE"])!=toString(x[i,"TARGET"])) {                            #If it's a cooperation event
      curr_dyad<-paste(toString(x[i,"SOURCE"]),toString(x[i,"TARGET"]),sep="-")          #Represent the current dyad as a single string containing the source and target RFID codes separated by an underscore
      if(length(prev_dyadfor)) {                                              #If this is not the first cooperation event in the dataset
        if(curr_dyad %in% c(prev_dyadfor,prev_dyadrev))                         #If the current dyad matches the previous dyad (regardless of which individual arrived at the feeder first)
        {
          dyvec<-c(dyvec,0)                                                #dyad has not changed
        } else {
          dyvec<-c(dyvec,1)                                           #dyad has changed
        }
      } else {
        dyvec<-c(dyvec,0)                                       #First cooperation event, so dyad cannot have changed
      }
      prev_dyadfor<-curr_dyad                                         #Current dyad becomes the reference dyad for the next cooperation event encountered by the loop
      prev_dyadrev<-paste(toString(x[i,"TARGET"]),toString(x[i,"SOURCE"]),sep="-")       #Both possible orientations of a dyad are required as references (we want to know if the dyad has changed, not bothered here about whether the member of the dyad that arrives at the feeder first changes)
    } else {
      dyvec<-c(dyvec,0)                                    #Dyad cannot have changed for non-dyadic events
    }
  }
  return(dyvec)
}



