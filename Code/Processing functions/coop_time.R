#MK Coop Data Timestamp

coop_time<-function(x) {
  tempstr<-toString(x[1])          #ensure that START_EXPERIMENT timestamp is a string
  year<-paste("20",substr(tempstr,1,2),sep="")       #get year (2019)
  month<-substr(tempstr,3,4)                        
  day<-substr(tempstr,5,6)
  hr<-substr(tempstr,7,8)
  min<-substr(tempstr,9,10)
  sec<-substr(tempstr,11,12)
  fullstr<-paste(paste(year,month,day,sep="-"),paste(hr,min,sec,sep=":"),sep=" ")     #Reconstruct timestamp string in format that can be fed to as.POSIXct
  cttime<-as.POSIXct(fullstr,format="%Y-%m-%d %H:%M:%S")                            #get Start of Experiment as POSIX object    
  return(list(secs=as.numeric(cttime),full=cttime,date=paste(year,month,day,sep="-")))     #Output time of the start of the experiment in seconds, the full timestamp as a string and the date
}



