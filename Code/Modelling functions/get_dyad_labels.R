
get_dyad_labels<-function(df){
  templist<-unique(apply(df,1,function(x) paste(x["SOURCE"],x["TARGET"],sep="_")))
  templist2<-unlist(lapply(strsplit(templist,"_"),function(x) paste(x[2],x[1],sep="_")))
  tldf1<-data.frame(COMBINATION=templist,PAIRING=1:length(templist))
  tldf2<-data.frame(COMBINATION=templist2,PAIRING=1:length(templist2))
  tldf<-rbind(tldf1,tldf2)
  pair_l<-lapply(split(tldf,tldf[,1]),function(x) data.frame(COMBINATION=unique(x[,"COMBINATION"]),PAIRING=min(x[,"PAIRING"])))
  pair_df<-do.call(rbind,pair_l)
  pair_df<-pair_df[order(pair_df[,"PAIRING"]),]
  outvec<-apply(df,1,function(x) pair_df[pair_df[,"COMBINATION"] %in% paste(x["SOURCE"],x["TARGET"],sep="_"),"PAIRING"])
  return(outvec)
}