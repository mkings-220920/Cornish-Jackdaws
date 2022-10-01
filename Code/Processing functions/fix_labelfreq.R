#MK fix missing departure labels in RFID datastream

fix_labelfreq<-function(x,ad_temp){

alocs<-ad_temp$alocs      #from the output of check_arrdep() function. Locations at which there is an Arrival label without a corresponding Departure label for Antenna A.
blocs<-ad_temp$blocs      #from the output of check_arrdep() function. Locations at which there is an Arrival label without a corresponding Departure label for Antenna B.
chka<-ad_temp$chka        #RFID datastream filtered to only include Arrival or departure labels for Antenna A.
chkb<-ad_temp$chkb        #RFID datastream filtered to only include Arrival or departure labels for Antenna B.

#ADD MISSING DEPARTURE LABELS AND ADD LABELS TO INDICATE THAT IMMEDIATE REPLACEMENT OF ONE INDIVIDUAL BY ANOTHER HAS OCCURRED AT A GIVEN ANTENNA 
atemplist<-list()
if(length(alocs)){                      #If there are missing departure labels for antenna A.
  for(i in 1:length(alocs)){             #For each missing label
    atemplist[[i]]<-depdisp_fill(alocs[i],chka)     #Call the departure/displacement fill sub-function. 
  }
} else {
  atemplist<-list(NULL)
}

btemplist<-list()
if(length(blocs)){                      #If there are missing departure labels for antenna B.
  for(i in 1:length(blocs)){            #For each missing label
    btemplist[[i]]<-depdisp_fill(blocs[i],chkb)        #Call the departure/displacement fill sub-function. 
  }
} else {
  btemplist<-list(NULL)
}


#If final entry in the dataframe is an ARRIVAL then need to add a final DEPARTURE label.
            
if(nrow(chka) & !is.null(atemplist)){
  a_alldf<-do.call(rbind,atemplist)
  if(chka[nrow(chka),2]=="TAG_ARRIVED_A"){                    #If the last label for antenna A is an ARRIVAL
    a_alldf<-rbind(a_alldf,data.frame(TS=chka[nrow(chka),1],LABEL="TAG_DEPARTED_A",ID=chka[nrow(chka),1],pos=chka[nrow(chka),"pos"],secs=chka[nrow(chka),"secs"]))    #Append a DEPARTURE label to the end of the dataframe
  }
  if(chka[1,2]=="TAG_DEPARTED_A"){        
    a_alldf<-rbind(a_alldf,data.frame(TS=chka[1,1],LABEL="TAG_ARRIVED_A",ID=chka[1,1],pos=chka[1,"pos"],secs=chka[1,"secs"]))   #If the first label for antenna A is a DEPARTURE Append an ARRIVAL label to the start of the dataframe
  }
} else {a_alldf<-NULL}

if(nrow(chkb) & !is.null(btemplist)){
  b_alldf<-do.call(rbind,btemplist)
  if(chkb[nrow(chkb),2]=="TAG_ARRIVED_B"){                      #If the last label for antenna B is an ARRIVAL
    b_alldf<-rbind(b_alldf,data.frame(TS=chkb[nrow(chkb),1],LABEL="TAG_DEPARTED_B",ID=chkb[nrow(chkb),1],pos=chkb[nrow(chkb),"pos"],secs=chkb[nrow(chkb),"secs"]))
    }
  if(chkb[1,2]=="TAG_DEPARTED_B"){                             #If the first label for antenna B is a DEPARTURE
    b_alldf<-rbind(b_alldf,data.frame(TS=chkb[1,1],LABEL="TAG_ARRIVED_B",ID=chkb[1,1],pos=chkb[1,"pos"],secs=chkb[1,"secs"]))  #Append an ARRIVAL label to the start of the dataframe
  }
} else {b_alldf<-NULL}

#append the additional labels to the main dataframe
append_df<-rbind(a_alldf,b_alldf)           #Combine the additional labels for both antennae
colnames(x)[1:3]<-c("TS","LABEL","ID")        #Update column names on original dataframe
if(!is.null(append_df)){
  findf<-rbind(x,append_df)                     #append the additional departure/replacement labels to the end of the original datastream
} else {findf<-x}                                  

return(findf)
}



