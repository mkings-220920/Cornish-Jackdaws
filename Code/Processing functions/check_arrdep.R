#FIND ANY ARRIVAL LABELS NOT SUBSEQUENTLY FOLLOWED BY A DEPARTURE CODE in the datastream

check_arrdep<-function(x){
  
#Separate arrivals and departures for each antenna to create two dataframes that will be used to check whether every arrival has a corresponding departure
chka<-x[x[,2] %in% c("TAG_ARRIVED_A","TAG_DEPARTED_A","ATAG_DEPARTED_A"),]     
chkb<-x[x[,2] %in% c("TAG_ARRIVED_B","TAG_DEPARTED_B","ATAG_DEPARTED_B"),]

if(nrow(chka)){
  alocs<-which(as.character(chka[1:(nrow(chka)-1),2])==as.character(chka[2:nrow(chka),2]))        #ARRIVAL and DEPARTURE labels should alternate. Find cases in which alternation does not occur for Antenna A.
} else {alocs<-c()}

if(nrow(chkb)){
  blocs<-which(as.character(chkb[1:(nrow(chkb)-1),2])==as.character(chkb[2:nrow(chkb),2]))        #ARRIVAL and DEPARTURE labels should alternate. Find cases in which alternation does not occur for Antenna B.
} else {blocs<-c()}

return(list(alocs=alocs,blocs=blocs,chka=chka,chkb=chkb))
}




