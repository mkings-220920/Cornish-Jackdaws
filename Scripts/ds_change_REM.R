#MK find door state changes

ds_change_REM<-function(x){
  prev_ds<-c()
  dyvec<-c()
  curr_ds<-c()
  for(i in 1:nrow(x)){             #For each event in the dataset
    curr_ds<-paste(toString(x[i,"lock_bool"]),toString(x[i,"high_bool"]),toString(x[i,"low_bool"]),sep="-")     #Get a string of nchar==5 indicating door state, where "0-0-1" refers to the default task state (only LOW quality reward doors open), "1-0-0" refers to the basic LOCKOUT state (all doors shut), "1-1-0" refers to a successful cooperation event during a LOCKOUT (only HIGH reward doors open) and "0-1-0" refers to a successful cooperation event during non-lockout (All doors open). Note, "0-1-0" actually refers to the state "0-1-1", but the low.bool column as originally formulated is kept for indicating when the feeder is in its default (no cooperation activity) state. 
    if(length(prev_ds)) {               #if this is not the first event
      if(curr_ds==prev_ds)              #If the door state of the current event is the same as that of the previous event
      {
        dyvec<-c(dyvec,0)               #door state has not changed
      } else {
        dyvec<-c(dyvec,1)               #door state has changed
      }
    } else {
      dyvec<-c(dyvec,0)                 #First event in dataset, door state cannot have changed
    }
    prev_ds<-curr_ds                    #Current event becomes reference event for next iteration of the loop
  } 
  return(dyvec)
}

