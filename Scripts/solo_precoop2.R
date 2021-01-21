#MK get solo and Pre-Cooperation events in Eventnet format 

solo_precoop2<-function(x){
  if(length(which(x[,2]=="TAG_ARRIVED_A")) & length(which(x[,2]=="TAG_ARRIVED_B"))){   #if there are events on both antennae in this datafile
    #FIND AND CLASSIFY EVENTS
    aevent_list<-Map(function(a,b) seq(a,b,1),a=which(x[,2]=="TAG_ARRIVED_A"),b=which(x[,2] %in% c("TAG_DEPARTED_A","ATAG_DEPARTED_A")))   #For antenna A, get list with elements consisting of the indices for each LOG entry corresponding to a given event (i.e. an event with consist of an ARRIVAL and DEPARTURE label and an event type label (e.g. STARTCOOP_SUCCESS))
    bevent_list<-Map(function(a,b) seq(a,b,1),a=which(x[,2]=="TAG_ARRIVED_B"),b=which(x[,2] %in% c("TAG_DEPARTED_B","ATAG_DEPARTED_B")))   #For antenna B, get list with elements consisting of the indices for each LOG entry corresponding to a given event (i.e. an event with consist of an ARRIVAL and DEPARTURE label and an event type label (e.g. STARTCOOP_SUCCESS))
    alabels<-lapply(aevent_list,function(a) if("SOLO_SUCCESS" %in% x[a,2] || "SOLO_FAIL" %in% x[a,2]) {if("STARTCOOP_SUCCESS" %in% x[a,2] || "STARTCOOP_FAIL" %in% x[a,2]) {"COOP_STARTER"} else {"SOLO"}} else if("STARTCOOP_SUCCESS" %in% x[a,2] || "STARTCOOP_FAIL" %in% x[a,2]) {"COOP_JOINER"} else {"ERROR"})   #For antenna A, go through the list of event indices (from above) and determine which of the events are cooperation events and label them accordingly. Note, a distinction is made between whether the cooperation event was initiated by (event LOG contains both a SOLO Label and a COOP event, labelled as COOP_STARTER) or joined by (COOP_JOINER) the individual on ANTENNA A during the current event
    blabels<-lapply(bevent_list,function(a) if("SOLO_SUCCESS" %in% x[a,2] || "SOLO_FAIL" %in% x[a,2]) {if("STARTCOOP_SUCCESS" %in% x[a,2] || "STARTCOOP_FAIL" %in% x[a,2]) {"COOP_STARTER"} else {"SOLO"}} else if("STARTCOOP_SUCCESS" %in% x[a,2] || "STARTCOOP_FAIL" %in% x[a,2]) {"COOP_JOINER"} else {"ERROR"})    #As above, but for antenna B
    
    #SOLO EVENTS
    soloa<-Map(function(a,b) if(a=="SOLO") {data.frame(SOURCE=x[b[1],3],TARGET=x[b[1],3],TIME=x[b[1],"secs"],TYPE="SOLO_NO_COOP",WEIGHT=as.numeric(x[b[length(b)],"secs"])-as.numeric(x[b[1],"secs"]),SITE=x[b[1],"SITE"],ANTENNA="A")} else {NULL},a=alabels,b=aevent_list)   #For solo events on Antenna A, get a list of dataframes, each containing a solo event formatted appropriately (i.e. for use in EVENTNET)
    solob<-Map(function(a,b) if(a=="SOLO") {data.frame(SOURCE=x[b[1],3],TARGET=x[b[1],3],TIME=x[b[1],"secs"],TYPE="SOLO_NO_COOP",WEIGHT=as.numeric(x[b[length(b)],"secs"])-as.numeric(x[b[1],"secs"]),SITE=x[b[1],"SITE"],ANTENNA="B")} else {NULL},a=blabels,b=bevent_list)   #As above, but for antenna B
    soloa_df<-do.call(rbind,soloa)               #Collapse list of dataframes into a single dataframe containing all of the formatted solo events on antenna A
    solob_df<-do.call(rbind,solob)               #As above, but for antenna B
    all_solonocoop<-rbind(soloa_df,solob_df)     #Combine solo events from antenna A and antenna B into a single dataframe containing all formatted solo events
    
    #ERRORS
    errors_a<-Map(function(a,b) if(a=="ERROR"){data.frame(FIRST=b[1],SECOND=b[length(b)])},a=alabels,b=aevent_list)  #Find the Onset and Offset times of events on Antenna A that did not have an event type label (i.e. had an Arrival and Departure label, but did not have an indicator of SOLO, COOPERATION etc)
    errors_b<-Map(function(a,b) if(a=="ERROR"){data.frame(FIRST=b[1],SECOND=b[length(b)])},a=blabels,b=bevent_list)   #As above, except for antenna B
    err_a_df<-do.call(rbind,errors_a)                #Get dataframe of locations of all erroneous events on antenna A
    err_b_df<-do.call(rbind,errors_b)                 #As above, except for antenna B
    all_errors<-rbind(err_a_df,err_b_df)             #Get dataframe containing locations of all erroneous events
    
    #PRE-COOPERATION (Time between first individual arriving at the feeder and arrival of the second, which triggers a cooperation event)
    pc_a_list<-lapply(aevent_list,function(a) x[a,])          #For Antenna A, get list of dataframes of events using the list of their locations
    pc_b_list<-lapply(bevent_list,function(a) x[a,])          #For Antenna B, get list of dataframes of events using the list of their locations
    coop_target_labels_a<-Map(function(a,b) if(a=="COOP_STARTER"){toString(b[which(b[,2] %in% c("TAG_ARRIVED_B"))[1],3])} else {NULL},a=alabels,b=pc_a_list)   #For antenna A, get RFID CODE of the individual that joined the event (i.e. landed on antenna B)
    coop_target_labels_b<-Map(function(a,b) if(a=="COOP_STARTER"){toString(b[which(b[,2] %in% c("TAG_ARRIVED_A"))[1],3])} else {NULL},a=blabels,b=pc_b_list)  #For antenna B, get RFID CODE of the individual that joined the event (i.e. landed on antenna A)
    precoop_a<-Map(function(a,b,d,h) if(a=="COOP_STARTER"){data.frame(SOURCE=x[b[1],3],TARGET=h,TIME=x[b[1],"secs"],TYPE="PRE_COOP",WEIGHT=as.numeric(d[which(x[b,2] %in% c("STARTCOOP_SUCCESS","STARTCOOP_FAIL"))[1],"secs"])-as.numeric(x[b[1],"secs"]),SITE=x[b[1],"SITE"],ANTENNA="A")} else {NULL},a=alabels,b=aevent_list,d=pc_a_list,h=coop_target_labels_a)    #For every event on antenna A for which the individual on Antenna A initiated a cooperation event (i.e. was the first to arrive), output a dataframe containing the RFID code of the individual on Antenna A, the individual that joined on antenna B, the duration between the arrival of an individual on antenna A and the onset of the cooperation event and other context info (Site, Antenna, Event Type)
    precoop_b<-Map(function(a,b,d,h) if(a=="COOP_STARTER"){data.frame(SOURCE=x[b[1],3],TARGET=h,TIME=x[b[1],"secs"],TYPE="PRE_COOP",WEIGHT=as.numeric(d[which(x[b,2] %in% c("STARTCOOP_SUCCESS","STARTCOOP_FAIL"))[1],"secs"])-as.numeric(x[b[1],"secs"]),SITE=x[b[1],"SITE"],ANTENNA="B")} else {NULL},a=blabels,b=bevent_list,d=pc_b_list,h=coop_target_labels_b)    #As above, except for antenna B
    pre_a_df<-do.call(rbind,precoop_a)           #Get a single dataframe of all cooperation events on antenna A that were initiated by an individual on antenna A
    pre_b_df<-do.call(rbind,precoop_b)           #As above, except for antenna B.
    all_pre<-rbind(pre_a_df,pre_b_df)           #Get a single dataframe containing the latencies from first arrival to onset for all cooperation events
    
    return(list(all_solonocoop,all_pre,all_errors))
    } else {
    return(NULL)
  }
}
















