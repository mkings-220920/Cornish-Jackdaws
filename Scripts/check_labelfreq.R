#MK check label frequencies

check_labelfreq<-function(x){

ftable<-as.data.frame(table(x[,2]))         #get frequencies for each label type
boola<-(ftable[ftable[,"Var1"]=="TAG_ARRIVED_A","Freq"])==(ftable[ftable[,"Var1"]=="TAG_DEPARTED_A","Freq"] + ftable[ftable[,"Var1"]=="ATAG_DEPARTED_A","Freq"])     #Check that the total number of arrivals and departures are equal for Antenna A 
boolb<-(ftable[ftable[,"Var1"]=="TAG_ARRIVED_B","Freq"])==(ftable[ftable[,"Var1"]=="TAG_DEPARTED_B","Freq"] + ftable[ftable[,"Var1"]=="ATAG_DEPARTED_B","Freq"])       #Check that the total number of arrivals and departures are equal for Antenna B         
tot_visits<-ftable[ftable[,"Var1"]=="TAG_ARRIVED_A","Freq"] + ftable[ftable[,"Var1"]=="TAG_ARRIVED_B","Freq"]      #Calculate total number of visits
tot_eventlabel<-ftable[ftable[,"Var1"]=="SOLO_SUCCESS","Freq"] + ftable[ftable[,"Var1"]=="SOLO_FAIL","Freq"] + ftable[ftable[,"Var1"]=="STARTCOOP_FAIL","Freq"] + ftable[ftable[,"Var1"]=="STARTCOOP_SUCCESS","Freq"]     #Calculate total number of event labels
boolc<-tot_visits==tot_eventlabel   #check that total number of visits equals total number of events

if(!length(boola)){
  boola<-NA  
}
if(!length(boolb)){
  boolb<-NA  
}
if(!length(boolc)){
  boolc<-NA  
}

out_df<-data.frame(File=f_name,matchA=boola,matchB=boolb,matchC=boolc,find_NA_secs=max(as.numeric(is.na(x[,"secs"]))))      #Output dataframe indicating whether the number of arrivals and departures match for both antennae, whether the total number of recorded visits equals the total number of recorded events and whether there are missing timestamps.
return(out_df)
}