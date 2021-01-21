#MK door state backfill

backfill_doors2<-function(x,y)                   #Determine what the tasks door states (HIGH reward doors open, only LOW reward doors open, LOCKOUT) were during every event, by inspecting event types and the timing of their onset and applying the door state rules (15 second minimum duration for HIGH OPEN, 120 second minimum duration for LOCKOUT, LOW OPEN by default (if neither HIGH OPEN or LOCKOUT) )
{
  highstart<-as.numeric(x[which(x[,"TYPE"]=="COOP_SUCCESS"),"TIME"])  #Times at which successful cooperation events began
  highdur<-as.numeric(x[which(x[,"TYPE"]=="COOP_SUCCESS"),"WEIGHT"])     #Duration of successful cooperation events 
  lockstart<-c(y,as.numeric(x[which(x[,"TYPE"]=="COOP_FAIL"),"TIME"]))    #Times at which unsuccessful cooperation events (and therefore lockout periods) began
  lockdur<-c(120,as.numeric(x[which(x[,"TYPE"]=="COOP_FAIL"),"WEIGHT"]))   #Duration of unsuccessful cooperation events. In addition, the first two minutes of the experiment was (unconditionally) a lockout
  highend1<-highstart+15    #Minimum time (seconds) at which all of the recorded successful cooperation events would end (15 second minimum HIGH (reward) OPEN door state)
  highend2<-highstart+highdur    #Times (seconds) at which dyads ceased to (successfully) interact.
  highend<-Map(function(a,b) if(a>=b) {a} else {b},a=highend1,b=highend2)  #HIGH reward doors marked as open unless the successful cooperation event lasted longer than 15 seconds (birds that triggered the successful event remain together at the task for longer than 15 seconds, then HIGH doors stay open until one of them leaves)
  lockend1<-lockstart+120    #Minimum time (seconds) at which all of the recorded lockout periods would end (120 second minimum lockout duration)
  lockend2<-lockstart+lockdur   #Times (seconds) at which dyads ceased to (unsuccessfully) interact.
  lockend<-Map(function(a,b) if(a>=b) {a} else {b},a=lockend1,b=lockend2)    #Lockout period 120 seconds unless the unsuccessful interaction lasted longer (birds that triggered the lockout do not leave within two minutes)
  highsecs<-unique(unlist(Map(function(a,b) seq(a,b,1),a=highstart,b=highend)))   #Get all times (seconds) that HIGH doors open 
  locksecs<-unique(unlist(Map(function(a,b) seq(a,b,1),a=lockstart,b=lockend)))    #Get all times (seconds) during which the task was in a LOCKOUT period
  lock_bool<-sapply(x[,"TIME"],function(a) if(a %in% locksecs) {1} else {0})     #get extra column indicating lockout vs non-lockout
  high_bool<-sapply(x[,"TIME"],function(a) if(a %in% highsecs) {1} else {0})      #get extra column indicating HIGH doors open vs HIGH doors closed
  low_bool<-unlist(Map(function(a,b) if(a || b){0} else {1},a=lock_bool,b=high_bool))    #get extra column indicating low doors open only (all times that were not a HIGH OPEN period or a lockout)
  out_df<-cbind(x,lock_bool,high_bool,low_bool)              #return the event dataframe with three new columns indicating which door state the task is in at the time of each event. Note, separate columns are required as HIGH OPEN and LOCKOUT can co-occur (i.e. successful interactions during lockouts were rewarded). Low_bool==1 if HIGH_bool==0 && LOCKOUT==0 and equals zero otherwise.
  return(out_df)
}















