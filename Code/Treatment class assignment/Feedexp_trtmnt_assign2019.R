library(reshape2)

pair_df<-read.csv("FeedExp_pairs100419.csv",header=T)
lh_df<-read.csv("LH_090419.csv",header=T)

#get all IDs in and process
tempdf<-lh_df[which(!lh_df[,"RFID"] %in% c("")),]   #remove entries with blank RFID code records
tempdf<-lh_df[order(as.Date(lh_df[,"DATE"],format="%d/%m/%Y")),]  #order by ringing date
rf_list<-lapply(split(tempdf,tempdf[,"ID"]),function(x) data.frame(ID=x[nrow(x),"ID"],RFID=x[nrow(x),"RFID"],SEX=x[nrow(x),"SEX"]))  #For each individual, get only its most recent RFID code
rf_df<-do.call(rbind,rf_list)  

other_df<-rf_df[which(!rf_df[,"ID"] %in% pair_df[,"ID"]),]   #find which RFID codes belong to individuals with a known breeding partner
other_df<-cbind(other_df,Pair=rep("",nrow(other_df)),Site=rep("",nrow(other_df)),Category=rep(5,nrow(other_df)))  #dummy info (empty strings) for individuals without a breeding partner
all_df<-rbind(pair_df,other_df)   #combine everything


#write.csv(all_df,file="fexp_all.csv")
all_df<-read.csv("fexp_all.csv",header=T)


#do the assignments (p_assign and other_assign functions can be found on lines 123 and 152 respectively):
#Stithians site
asgn1_stith<-p_assign(all_df[which(all_df[,"Category"]==1 & all_df[,"Site"]=="STITHIANS"),])   #assignment for known pairs with functional tags on both members
asgn2_stith<-p_assign(all_df[which(all_df[,"Category"]==2 & all_df[,"Site"]=="STITHIANS"),])   #assignment for known pairs where one or both don't have functional tags
asgn3_stith<-p_assign(all_df[which(all_df[,"Category"]==3 & all_df[,"Site"]=="STITHIANS"),])   #assignment for possible (i.e. unverified) pairs
asgn4_stith<-other_assign(all_df[which(all_df[,"Category"]==4 & all_df[,"Site"]=="STITHIANS"),])   #assignment for unpaired individuals that are regular passive RFID feeder users

#Pencoose site
asgn1_penc<-p_assign(all_df[which(all_df[,"Category"]==1 & all_df[,"Site"]=="PENCOOSE"),])   #assignment for known pairs with functional tags on both members
asgn2_penc<-p_assign(all_df[which(all_df[,"Category"]==2 & all_df[,"Site"]=="PENCOOSE"),])   #assignment for known pairs where one or both don't have functional tags
asgn3_penc<-p_assign(all_df[which(all_df[,"Category"]==3 & all_df[,"Site"]=="PENCOOSE"),])   #assignment for possible (i.e. unverified) pairs
asgn4_penc<-other_assign(all_df[which(all_df[,"Category"]==4 & all_df[,"Site"]=="PENCOOSE"),]) #assignment for unpaired individuals that are regular passive RFID feeder users

asgn5<-other_assign(all_df[which(all_df[,"Category"]==5),])   #assignment for remaining individuals

outdf<-rbind(asgn1_penc,asgn2_penc,asgn3_penc,asgn4_penc,asgn1_stith,asgn2_stith,asgn3_stith,asgn4_stith,asgn5)
#write.csv(outdf,file="outdf110419.csv")




#Clean-up:

df2<-read.csv("outdf110419.csv",header=T)
#bagin<-df2[which(as.character(df2[,"RFID"]) %in% c("")),]
#bagm<-merge(bagin,rf_df,by="ID")


rfid_vec<-as.character(tolower(df2[,"RFID"]))   #Change RFID code characters to lower case
alt_tag<-sapply(rfid_vec,function(x) if(toString(x) %in% c("")) {NA} else if(substr(toString(x),1,2) %in% c("07")) {paste("07",substr(toString(x),(nchar(toString(x))-3),nchar(toString(x))),"00",sep="")} else {paste("01",substr(toString(x),(nchar(toString(x))-3),nchar(toString(x))),"00",sep="")})   #RFID tag codes need to be re-formatted from 10 characters down to 8 characters for use by the Darwin Board. Here combine the prefix of the tag batch ID with each codes unique 4 character sequence and then add two additional dummy characters "00"
df2<-cbind(df2,alt_tag)
#write.csv(df2,file="df2_110419.csv")


#find df2 individuals ringed in 2016/2017 (during which a batch of RFID-tags was used that had high failure rates (tag death))
year<-apply(lh_df,1,function(x) substr(toString(x["DATE"]),7,10))
searchdf<-cbind(lh_df,year)
deadtag<-searchdf[which(searchdf[,"CODE"]=="RINGED" & (searchdf[,"year"] %in% c("2016","2017"))),"ID"]  #Find individuals ringed in 2016/2017
keepers<-which((!df2[,"ID"] %in% deadtag) | (df2[,"Category"] %in% c(1,2,3,4)))  #Keep (ringed) individuals that either were not ringed during the above period, or were but belong to an important category (i.e. have a known breeding partner)
df3<-df2[keepers,]


consdf<-read.csv("consolation.csv",header=T)   #Read in Beki's pair data
tmnt_df<-df3[,c("ID","TREATMENT")]
check_df<-merge(consdf,tmnt_df,by="ID")
meltdf<-melt(check_df,id.vars=c("Pair","SEX"),measure.vars="TREATMENT")
castdf<-dcast(meltdf,SEX ~ Pair,value.var="value")


grp1<-df3[which(df3[,"TREATMENT"]==1),"alt_tag"]   #Get vector of tag codes for individuals assigned to class 1/class A
grp1<-as.character(grp1[which(!is.na(grp1))])

grp2<-df3[which(df3[,"TREATMENT"]==2),"alt_tag"]   #Get vector of tag codes for individuals assigned to class 2/class B
grp2<-as.character(grp2[which(!is.na(grp2))])

dups<-grp1[which(grp1 %in% grp2)]  #Check for duplication (i.e. codes found in both class vectors)
grp1<-grp1[which(!grp1 %in% dups)]   #Make sure that duplicates removed
grp2<-grp2[which(!grp2 %in% dups)]    #Make sure that duplicates removed

dsamp<-sample(1:2,length(dups),rep=T)   #If there are duplicates, now re-assign each of them to just one class
d1<-as.character(dups[dsamp==1])
d2<-as.character(dups[dsamp==2])

grp1<-c(grp1,d1)
grp2<-c(grp2,d2)
grp1<-unique(grp1)  
grp2<-unique(grp2)


#Create text files. One text file per treatment class. Each file contains only the RFID codes of the individuals assigned to it (one RFID code per line)
for(i in 1:length(grp1)){
  filename<-paste(grp1[i],".txt",sep="")
  fileConn1<-file(filename)
  writeLines("1",fileConn1)
  close(fileConn1)
}


for(i in 1:length(grp2)){
  filename<-paste(grp2[i],".txt",sep="")
  fileConn1<-file(filename)
  writeLines("2",fileConn1)
  close(fileConn1)
}



fileConn1<-file("coopgrp1.txt")
writeLines(grp1,fileConn1)
close(fileConn1)

fileConn2<-file("coopgrp2.txt")
writeLines(grp2,fileConn2)
close(fileConn2)



###########
#FUNCTIONS#
###########


p_assign<-function(x)  #Treatment class assignment for breeding pairs
{
  pairs<-unique(x[,"Pair"])  #get vector of unique pair numbers
  num_pairs<-length(pairs)   #calculate number of pairs to assign
  clipval<-(floor(num_pairs/4)*4)      #Four possible treatment class combinations - both members of a pair assigned to class A/class 1, both members assigned to class B/class 2, male assigned to class A female to B, male assigned to B female to A. Calculate nearest multiple of four to the number of pairs - equalize number of pairs belonging to each class combination as much as possible.
  sampvec1<-c(rep(1,(clipval/4)),rep(2,(clipval/4)),rep(3,(clipval/4)),rep(4,(clipval/4)))   #vector with an equal number of each of the four class combinations (see above). Vector length given by nearest multiple of four to number of pairs (that is also less than the number of pairs)
  p_assgn1<-sample(sampvec1,clipval,rep=FALSE)    #random shuffle of vector of class combination codes.
  pdf1<-data.frame(Pair=pairs[1:clipval],ASSIGNMENT=p_assgn1)   #store the above
  p_assgn2<-sample(1:4,(num_pairs-clipval),rep=T)           #If number of pairs is not a multiple of four, then the remainder must be dealt with. Assign class combination at random.
  pdf2<-data.frame(Pair=pairs[(clipval+1):num_pairs],ASSIGNMENT=p_assgn2) #Store the above
  pdf<-rbind(pdf1,pdf2)       #Combine to get a dataframe of randomly assigned treatment class combinations for all pairs
  findf<-merge(x,pdf,by="Pair")    #Merge with input dataframe
  sexdf<-findf[findf[,"SEX"] %in% c("M","F"),]     #First evaluate class assignments for individuals of known sex...
  trmnts<-apply(sexdf,1,function(x) if(x["ASSIGNMENT"]==1){1} else if(x["ASSIGNMENT"]==2){2} else if(x["ASSIGNMENT"]==3 && x["SEX"]=="M") {1} else if(x["ASSIGNMENT"]==3 && x["SEX"]=="F"){2} else if(x["ASSIGNMENT"]==4 && x["SEX"]=="M") {2} else if(x["ASSIGNMENT"]==4 && x["SEX"]=="F"){1})   #Convert each pairs class-combination assignment into treatment class assignments for individuals
  sdf<-cbind(sexdf,TREATMENT=trmnts)  #store individual's class assignments
  blankdf<-findf[findf[,"SEX"] %in% c(""),]    #Next, evaluate individuals for which sex is currently not known...
  if(nrow(blankdf))   #If there are such individuals...
  {
  blist<-lapply(split(blankdf,blankdf[,"Pair"]),function(x) if(x[1,"ASSIGNMENT"]==1) {data.frame(x,TREATMENT=c(1,1))} else if(x[1,"ASSIGNMENT"]==2) {data.frame(x,TREATMENT=c(2,2))} else {data.frame(x,TREATMENT=sample(1:2,2,rep=F))})  #Go through each pair for which the sex of one or both of the individuals is not known. If they are both to be assigned to the same class, perform the assignment. Otherwise, assign them to different classes at random.
  bdf<-do.call(rbind,blist)   
  findf<-rbind(sdf,bdf)   
  }
  else{
    findf<-sdf
  }
  return(findf)
}


other_assign<-function(x)   #Treatment class assignment for individuals without a known breeding partner
{
  mdf<-x[which(x[,"SEX"]=="M"),]  #get males
  fdf<-x[which(x[,"SEX"]=="F"),]  #get females
  blankdf<-x[which(x[,"SEX"]==""),]   #get remaining individuals for which sex is currently known
  num_m<-nrow(mdf)   #calculate number of males
  num_f<-nrow(fdf)    #calculate number of females
  num_blank<-nrow(blankdf)    #calculate number of individuals for which sex is currently unknown
  clipm<-(floor(num_m/2)*2)    #Calculate nearest (lower) multiple of two for number of males
  clipf<-(floor(num_f/2)*2)    #Calculate nearest (lower) multiple of two for number of females
  clipblank<-(floor(num_blank/2)*2)  #Calculate nearest (lower) multiple of two for individuals for which sex is not known
  msample1<-sample(c(rep(1,(clipm/2)),rep(2,(clipm/2))),clipm,rep=FALSE)   #create a vector with equal number of class 1s and class 2s for males (excluding remainder) then shuffle it 
  fsample1<-sample(c(rep(1,(clipf/2)),rep(2,(clipf/2))),clipf,rep=FALSE)    #create a vector with equal number of class 1s and class 2s for females (excluding remainder) then shuffle it 
  bsample1<-sample(c(rep(1,(clipblank/2)),rep(2,(clipblank/2))),clipblank,rep=FALSE)  #create a vector with equal number of class 1s and class 2s individuals for which sex is not known (excluding remainder) then shuffle it 
  msample2<-sample(1:2,(num_m-clipm),rep=T)   #randomly assign classes for remaining males
  fsample2<-sample(1:2,(num_f-clipf),rep=T)    #randomly assign classes for remaining females
  bsample2<-sample(1:2,(num_blank-clipblank),rep=T) #randomly assign classes for remaining individuals for which sex not known
  mdf<-cbind(mdf,TREATMENT=c(msample1,msample2))
  fdf<-cbind(fdf,TREATMENT=c(fsample1,fsample2))
  blankdf<-cbind(blankdf,TREATMENT=c(bsample1,bsample2))
  outdf<-rbind(mdf,fdf,blankdf) 
  outdf<-cbind(outdf,ASSIGNMENT=rep(NA,nrow(outdf)))   #dummy column added (to match p_assign function output)
}






