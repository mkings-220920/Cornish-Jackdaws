/******************************************************************************
 * Title:            Darwin Board Dual-Feeder Experiment (2019)
 * Author:           Martin Whitaker (alterations/edits by Michael Kings)
 * Target uC:        LPC2106     
 * Copyright:        Stickman Technology
 * Compile with:     WinARM toolkit 20060606 (which includes newlib-lpc rel 5a)
 *****************************************************************************/

#ifdef EXETER_BUILD

/////////////
//LIBRARIES//
/////////////

#include "feeder_library.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "feeder_api.h"
#include "ff.h"

/////////////
//FUNCTIONS//
/////////////

//WRITE EVENT LABEL AND TIME TO LOG FILE
static void WriteExeterLogEntry(char *event, char *tag)
{
	static char event_text[200];
	struct timestamp tmst;

	ReadRTC(&tmst);
	siprintf(event_text, "%02d%02d%02d%02d%02d%02d",
				tmst.year, tmst.month, tmst.day,
				tmst.hour, tmst.min, tmst.sec);
	siprintf(event_text+12,",%s,%s",event,tag);
	siprintf(event_text+strlen(event_text),"\n");
	iprintf("LOG: %s\r",event_text);
	writeDataToFile("exeter.csv",event_text,strlen(event_text));
}



//READ ONE LINE FROM .txt FILE
int ReadOneLine(FIL *InFile, char *output, int maxlen)
{
	FRESULT Res;
	unsigned int BytesRead;
	int i=0;
	int fail=0;
	while ((i<maxlen)&&(fail==0)) {
		Res = f_read(InFile, output+i, 1, &BytesRead);
		if (Res != FR_OK || BytesRead == 0) {
			output[i]=0;
			fail=1;
		} else {
			if (output[i]=='\n') {
				output[i]=0;
				i=maxlen;
			} else {
				if ((output[i]>='A')&&(output[i]<='Z')) output+=32;
				if (((output[i]>='a')&&(output[i]<='z'))||((output[i]>='0')&&(output[i]<='9'))) i++;
			}
		}
	}
	// Always null terminate at the very end to be sure
	output[maxlen-1]=0;
	return fail;
}


//CHECK GROUP MEMBERSHIP 
int CheckGRP1(char *taga)    //Check if tag is in group 1
{
    char filename[MAX_TAG_LEN+5];
    char oneline[40];
    int fail=0;
    FIL InFile;
    FRESULT Res;
    int foundtag=0;
  
    // Returns 1 if this is a special pair, otherwise 0
    if (taga[0]==0) return 0;

    siprintf(filename,"coopgrp1.txt");
  
    // Open file for reading
    Res = f_open(&InFile, filename, FA_READ);
    if (Res != FR_OK)
    {
        iprintf("coopgrp1.txt not found\n\r");
        return 0;
    }
  
    // File exists - scan data for other tag
    fail=ReadOneLine(&InFile, oneline, 40);
    while ((fail==0)&&(foundtag==0)) {
        //iprintf("Checking for match %s = %s\n\r",oneline,taga);
        if (strcmp(oneline,taga)==0) {
            //iprintf("Match found");
            foundtag=1;
        }
        fail=ReadOneLine(&InFile, oneline, 40);
    }

    f_close(&InFile);
  
    return foundtag;
}


int CheckGRP2(char *taga)  //check if tag is in group 2
{
    char filename[MAX_TAG_LEN+5];
    char oneline[40];
    int fail=0;
    FIL InFile;
    FRESULT Res;
    int foundtag=0;
  
    // Returns 1 if this is a special pair, otherwise 0
    if (taga[0]==0) return 0;

    siprintf(filename,"coopgrp2.txt");
  
    // Open file for reading
    Res = f_open(&InFile, filename, FA_READ);
    if (Res != FR_OK)
    {
        iprintf("coopgrp2.txt not found\n\r");
        return 0;
    }
  
    // File exists - scan data for other tag
    fail=ReadOneLine(&InFile, oneline, 40);
    while ((fail==0)&&(foundtag==0)) {
        //iprintf("Checking for match %s = %s\n\r",oneline,taga);
        if (strcmp(oneline,taga)==0) {
           // iprintf("Match found");
            foundtag=1;
        }
        fail=ReadOneLine(&InFile, oneline, 40);
    }

    f_close(&InFile);
  
    return foundtag;
}


//CONTROL SERVOS
void SetDoorsMotor(int doora, int doorb, int doorc, int doord, int opendoor, int angle)
{
	if(opendoor){
		if (doora) pca9685_setpos(0, doora);
		if (doorb) pca9685_setpos(1, doorb);
		if (doorc) pca9685_setpos(2, doorc);
		if (doord) pca9685_setpos(3, doord);
	} else {
		if (doora) pca9685_setpos(0, angle-doora);
		if (doorb) pca9685_setpos(1, angle-doorb);
		if (doorc) pca9685_setpos(2, angle-doorc);
		if (doord) pca9685_setpos(3, angle-doord);
	}
}



/////////////
//MAIN LOOP//
/////////////

void LaunchFullExperiment(void)
{
	//INITIALIZATION
	//Door states (current and previous; high and low)
	int door_state_low=1;   //Numeric. Tracks what the LOW doors should be doing (0 = closed; 1 = open)
	int previous_door_state_low=-1;
	int door_state_high=0;   //Numeric. Tracks what the HIGH doors should be doing (0 = closed; 1 = open)
	int previous_door_state_high=-1;

	//Tag IDs. Current and previous, antenna A and antenna B
	char current_tag_a[MAX_TAG_LEN];
	char current_tag_b[MAX_TAG_LEN];
	char previous_tag_a[MAX_TAG_LEN];
	char previous_tag_b[MAX_TAG_LEN];
	previous_tag_a[0]=0;  //initially empty
	current_tag_a[0]=0;
	previous_tag_b[0]=0;
	current_tag_b[0]=0;
	int tag_grp_a=0;
	int tag_grp_b=0;
	char coop_outstring[(MAX_TAG_LEN*2)+1];
	
	
	//Experimental design
	int lock_thresh=120;     //Numeric. Duration of lockout (in seconds). Lockout is a period of all doors closed following a failed cooperation attempt.
	int high_min=15;			//Numeric. Minimum duration that HIGH doors must be open for (in seconds).
	resetMicroClock();
	int failtime=readMicroClock()/100;
	int hightime=readMicroClock()/100;	
	int solotime=readMicroClock()/100;
	int lockout=1;
	
	//Doors
	int servo_angle=75;     //Angle (in degrees) of doors  when open (note closed is zero)
	int low_door_progress=0;   //Logical. Are LOW doors in the process of opening/closing?
	int high_door_progress=0;	//Logical. Are HIGH doors in the process of opening/closing?
	int i=0;  //Numeric. Pointer for keeping track of LOW door position. Angle of door in degrees.
	int j=0;  //Numeric. Pointer for keeping track of HIGH door position. Angle of door in degrees.
	int doorpoint_l=1;
	int doorpoint_h=1;
	int open_speed=12;
	
	
	//Miscellaneous 
	int experiment_running=1;   //Logical. Should experiment continue to run? Low battery is the only scenario that changes this.
	int success_count=0;   //count of number of successful cooperation events
	int solo_count=0;
	int failure_count=0;
	char temp_text[20];		//temporary store for strings to be displayed on LCD
	
	
	//PREPARATION
	//Check the servos
	ClearScreen();
	if (pca9685_reset()!=0) {
		WriteScreen(0, 0, "Servo Fail!");
	} else {
		WriteScreen(0, 0, "Servo OK");
	}
	WaitUs(1000000);

	//Set up the display
	ClearScreen();
	WriteScreen(0, 0, " TAGA: ");
	WriteScreen(1, 0, " TAGB: ");
	WriteScreen(2, 0, " Solos: ");
	WriteScreen(3, 0, " Successes: ");
	WriteScreen(4, 0, " Failures: ");
	siprintf(temp_text,"%d",solo_count);
	WriteScreen(2, 12, temp_text);
	siprintf(temp_text,"%d",success_count);
	WriteScreen(3, 12, temp_text);
	siprintf(temp_text,"%d",failure_count);
	WriteScreen(4, 12, temp_text);
	
	//Note start of experimental period in LOG file (EXETER.csv)
	WriteExeterLogEntry("START_EXPERIMENT","");
	
	
	while(experiment_running==1) {
		GetBatteryStatus();    //Check battery life
		ReadTags(current_tag_a,current_tag_b,0,0);   //Get tag IDs of current antennae occupants. Note, can be blank.
		if (strcmp(current_tag_a,previous_tag_a)) {   //If current tag on antenna A differs from previous.
			if (current_tag_a[0]==0) {              //If current tag on antenna A is blank (but previous wasn't)
				// A tag has left - log it
				WriteExeterLogEntry("TAG_DEPARTED_A",previous_tag_a);
				WriteScreen(0, 8, "          ");
				if((failtime<solotime) && (hightime<solotime)){
					solo_count++;
					siprintf(temp_text,"%d",solo_count);   //print number of successful events on screen.
					WriteScreen(2, 12, temp_text);	
				}
			} else {
				// A tag has arrived - log it
				WriteExeterLogEntry("TAG_ARRIVED_A",current_tag_a);
				WriteScreen(0, 8, current_tag_a);
				if(CheckGRP1(current_tag_a)==1){
					tag_grp_a=1;
				} else if (CheckGRP2(current_tag_a)==1) {
					tag_grp_a=2;
				} else {
					tag_grp_a=3;
					WriteExeterLogEntry("TAG_MISSING_A",current_tag_a);
				}
				if((current_tag_b[0]!=0) && strcmp(current_tag_a,current_tag_b) && (tag_grp_a==tag_grp_b) && (tag_grp_a!=3) && (tag_grp_b!=3)){   //If occupants of both antennae are in the SAME treatment group
					hightime=readMicroClock()/100;    //Save start time of successful cooperation event to file.
					if((hightime-failtime)>lock_thresh){
						door_state_low=1;	//LOW doors open (open all doors)
						door_state_high=1;  //HIGH doors open  (open all doors)
					} else {
						door_state_low=0;
						door_state_high=1;  //HIGH doors open  (open all doors)	
					}
					success_count++;    //increase count of number of successful cooperation events by 1.
					siprintf(temp_text,"%d",success_count);   //print number of successful events on screen.
					WriteScreen(3, 12, temp_text);	
					siprintf(coop_outstring,"%s_%s",current_tag_b,current_tag_a);
					WriteExeterLogEntry("STARTCOOP_SUCCESS",coop_outstring);    //Log onset of successful cooperation event.
				} else if((current_tag_b[0]!=0) && strcmp(current_tag_a,current_tag_b) && (tag_grp_a!=3) && (tag_grp_b!=3)){     //If other antenna is not empty and current occupant of this antenna is not also registered on the other (this can occur in the case of fast solo perch-switching events)
					failtime=readMicroClock()/100;     //Save start time of failed cooperation event to file.
					door_state_low=0;  //LOW doors shut (close all doors)
					door_state_high=0;  //HIGH doors shut  (close all doors)
					lockout=1;
					failure_count++;    
					siprintf(temp_text,"%d",failure_count);   
					WriteScreen(4, 12, temp_text);	
					siprintf(coop_outstring,"%s_%s",current_tag_b,current_tag_a);
					WriteExeterLogEntry("STARTCOOP_FAIL",coop_outstring);   //Log the failure
				} else if(((readMicroClock()/100)-failtime)>lock_thresh){	//If occupant is engaging in a SOLO feeding event and there is no lockout (period of task inactivity) in effect
					solotime=readMicroClock()/100;   
					door_state_low=1;   //LOW doors open (just LOW open)
					door_state_high=0;   //HIGH doors shut  (just LOW open)
					WriteExeterLogEntry("SOLO_SUCCESS",current_tag_a);   //Log the successful SOLO event
				} else {	//Otherwise, a solo feeding event has occurred within the lockout period.
					solotime=readMicroClock()/100; 
					door_state_low=0;  //LOW doors shut  (close all doors)
					door_state_high=0;   //HIGH doors shut  (close all doors)
					WriteExeterLogEntry("SOLO_FAIL",current_tag_a);	  //Log the failed SOLO event
				} 
			}
			strcpy(previous_tag_a,current_tag_a);    //update previous tag ID to the current one (Antenna A)
		}   else {									//Current tag on antenna A DOES NOT differ from previous
			if((door_state_low==0) && (door_state_high==0) && (((readMicroClock()/100)-failtime)>lock_thresh)){  //If all doors are shut, but lockout no longer in effect
				door_state_low=1;	//LOW doors open
				door_state_high=0;   //HIGH doors shut
				lockout=0;
				WriteExeterLogEntry("LOCKOUT_ENDED","");   //Note end of lockout
			} else if((current_tag_b[0]==0) && (door_state_high==1)){  //if other individual has left following successful cooperation event
				if(strcmp(current_tag_b,previous_tag_b)){				//if the departure has just happened
					siprintf(coop_outstring,"%s_%s",previous_tag_b,current_tag_a);
					WriteExeterLogEntry("ENDCOOP_SUCCESS",coop_outstring);                  //Note the end of the successful cooperation event
				}
				if(((readMicroClock()/100)-hightime)>high_min){                 //If the HIGH doors have been open for at least the minimum amount of time
					if(((readMicroClock()/100)-failtime)>lock_thresh){                                    //If the successful cooperation event occurred within a lockout period
						door_state_low=1;	//LOW doors open
						door_state_high=0;  //HIGH doors shut
					} else {
						door_state_low=0;    //LOW doors shut
						door_state_high=0;   //HIGH doors shut
					}
				}
			} else if((current_tag_b[0]==0) && (door_state_low==0) && (door_state_high==0) && strcmp(current_tag_b,previous_tag_b)){
				siprintf(coop_outstring,"%s_%s",previous_tag_b,current_tag_a);
				WriteExeterLogEntry("ENDCOOP_FAIL",coop_outstring);  
			}
			
		}
		
		
		
		if (strcmp(current_tag_b,previous_tag_b)) {    //If current tag on antenna B differs from previous.
			if (current_tag_b[0]==0) {					//If current tag on antenna B is blank (but previous wasn't)
				// A tag has left - log it
				WriteExeterLogEntry("TAG_DEPARTED_B",previous_tag_b);
				WriteScreen(1, 8, "          ");
				if((failtime<solotime) && (hightime<solotime)){
					solo_count++;
					siprintf(temp_text,"%d",solo_count);   //print number of successful events on screen.
					WriteScreen(2, 12, temp_text);	
				} 
			} else {
				// A tag has arrived - log it
				WriteExeterLogEntry("TAG_ARRIVED_B",current_tag_b);
				WriteScreen(1, 8, current_tag_b);
				if(CheckGRP1(current_tag_b)==1){
					tag_grp_b=1;
				} else if (CheckGRP2(current_tag_b)==1) {
					tag_grp_b=2;
				} else {
					tag_grp_b=3;
					WriteExeterLogEntry("TAG_MISSING_B",current_tag_b);
				}
				if((current_tag_a[0]!=0) && strcmp(current_tag_a,current_tag_b) && (tag_grp_a==tag_grp_b) && (tag_grp_a!=3) && (tag_grp_b!=3)){  //If occupants of both antennae are in the SAME treatment group
					hightime=readMicroClock()/100;    //Save start time of failed cooperation event to file.
					if((hightime-failtime)>lock_thresh){
						door_state_low=1;	//LOW doors open (open all doors)
						door_state_high=1;  //HIGH doors open  (open all doors)
					} else {
						door_state_low=0;
						door_state_high=1;  //HIGH doors open  (open all doors)	
					}
					success_count++;    //increase count of number of successful cooperation events by 1.
					siprintf(temp_text,"%d",success_count);   //print number of successful events on screen.
					WriteScreen(3, 12, temp_text);	
					siprintf(coop_outstring,"%s_%s",current_tag_a,current_tag_b);
					WriteExeterLogEntry("STARTCOOP_SUCCESS",coop_outstring);  //Log onset of successful cooperation event.
				} else if((current_tag_a[0]!=0) && strcmp(current_tag_a,current_tag_b) && (tag_grp_a!=3) && (tag_grp_b!=3)){     //If other antenna is not empty and current occupant of this antenna is not also registered on the other (this can occur in the case of fast solo perch-switching events)
					failtime=readMicroClock()/100;     //Save start time of failed cooperation event to file.
					door_state_low=0;    //LOW doors shut  (shut all doors)
					door_state_high=0;   //HIGH doors shut  (shut all doors)
					lockout=1;
					failure_count++;    
					siprintf(temp_text,"%d",failure_count);   
					WriteScreen(4, 12, temp_text);	
					siprintf(coop_outstring,"%s_%s",current_tag_a,current_tag_b);
					WriteExeterLogEntry("STARTCOOP_FAIL",coop_outstring);   //Log the failure
				} else if(((readMicroClock()/100)-failtime)>lock_thresh){   //If all doors are shut, but lockout no longer in effect
					solotime=readMicroClock()/100;
					door_state_low=1;    //LOW doors open  (just LOW open)
					door_state_high=0;   //HIGH doors shut  (just LOW open)
					WriteExeterLogEntry("SOLO_SUCCESS",current_tag_b);   //Log the successful SOLO event
				} else {							//Otherwise, a solo feeding event has occurred within the lockout period (provided a treatme
					solotime=readMicroClock()/100;
					door_state_low=0;    //LOW doors shut   (shut all doors)
					door_state_high=0;   //HIGH doors shut   (shut all doors)
					WriteExeterLogEntry("SOLO_FAIL",current_tag_b);   //Log the failed SOLO event
				}
			}
			strcpy(previous_tag_b,current_tag_b);       //update previous tag ID to the current one (Antenna B)
		}   else {											//Current tag on antenna A DOES NOT differ from previous
			if((door_state_low==0) && (door_state_high==0) && (((readMicroClock()/100)-failtime)>lock_thresh)){   //If all doors are shut, but lockout no longer in effect
				door_state_low=1;		//LOW doors open
				door_state_high=0; 		//HIGH doors shut
				lockout=0;
				WriteExeterLogEntry("LOCKOUT_ENDED","");		//Note end of lockout
			} else if((current_tag_a[0]==0) && (door_state_high==1)){  	//if other individual has left following successful cooperation event
				if(strcmp(current_tag_a,previous_tag_a)){   			//if the departure has just happened
					siprintf(coop_outstring,"%s_%s",previous_tag_a,current_tag_b);
					WriteExeterLogEntry("ENDCOOP_SUCCESS",coop_outstring);					//Note the end of the successful cooperation event
				}
				if(((readMicroClock()/100)-hightime)>high_min){			//If the HIGH doors have been open for at least the minimum amount of time
					if(((readMicroClock()/100)-failtime)>lock_thresh){								//If the successful cooperation event occurred within a lockout period
						door_state_low=1;		//LOW doors open
						door_state_high=0;  	//HIGH doors shut
					} else {
						door_state_low=0;		//LOW doors open
						door_state_high=0;		//HIGH doors shut
					}
				}
			} else if((current_tag_a[0]==0) && strcmp(current_tag_a,previous_tag_a) && (door_state_low==0) && (door_state_high==0)){
				siprintf(coop_outstring,"%s_%s",previous_tag_a,current_tag_b);
				WriteExeterLogEntry("ENDCOOP_FAIL",coop_outstring);	
			}
		}
		
		
		//LOW DOORS
		if (door_state_low!=previous_door_state_low) {     //If high door state has changed
			if(low_door_progress==0){    //If the LOW doors are stationary
				low_door_progress=1;     //Trigger onset of door movement
				i=0;						//Doors will move over full range 
			} else {					//LOW doors are moving
				i=servo_angle-i;      //Begin next period of movement from current door position (Note, will be going the other way)
			}
			previous_door_state_low=door_state_low;     //Update LOW door state 
		}
		
		if(low_door_progress){      //If LOW doors are currently, or should be, in motion
			if(i<=servo_angle){			//if full range of motion not completed
				switch (door_state_low) {
					case 0: // Shut LOW doors
							SetDoorsMotor(i,i,0,0,0,servo_angle);   //Set servo (i.e. door position) to angle servo_angle-i 
							i=i+2;
							break;
					case 1: // Open LOW doors
							if(doorpoint_l<open_speed){
								doorpoint_l++;
							} else {
								SetDoorsMotor(i,i,0,0,1,servo_angle);	//Set servo (i.e. door position) to angle i
								doorpoint_l=1;
								i++;
							}
							break;
					}	
				}
			else {     //end motion
				low_door_progress=0;			
			}		
		}
		
		
		
		//HIGH DOORS
		if (door_state_high!=previous_door_state_high) {   //If high door state has changed
			if(high_door_progress==0){   //If the HIGH doors are stationary
				high_door_progress=1;    //Trigger onset of door movement
				j=0;						//Doors will move over full range 
			} else {                    //HIGH doors are moving
				j=servo_angle-j;		//Begin next period of movement from current door position (Note, will be going the other way)
			}
			previous_door_state_high=door_state_high;   //Update HIGH door state 
		}
		
		if(high_door_progress){  //if doors are currently in the process of opening/closing
			if(j<=servo_angle){     //if doors are not fully open/closed
			
				switch (door_state_high) {
					case 0: // Shut HIGH doors
							if(lockout){
								SetDoorsMotor(0,0,j,j,0,servo_angle);     //Set servo (i.e. door position) to angle servo_angle-i 
								j=j+2;
							} else if(doorpoint_h<=open_speed){
								doorpoint_h++;
							} else {
								SetDoorsMotor(0,0,j,j,0,servo_angle);     //Set servo (i.e. door position) to angle servo_angle-i 
								doorpoint_h=1;
								j++;
							}
							break;
					case 1: // Open HIGH doors
							if(doorpoint_h<=open_speed){
								doorpoint_h++;
							} else {
								SetDoorsMotor(0,0,j,j,1,servo_angle);     //Set servo (i.e. door position) to angle i
								doorpoint_h=1;
								j++;
							}
							break;
					}	
				}
			else {     //end motion
				high_door_progress=0;			
			}		
		}
		
		
		
		

	}

	return 0;
}

#endif

