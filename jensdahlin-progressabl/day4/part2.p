
/*------------------------------------------------------------------------
    File        : part1.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Dec 04 08:55:10 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttData NO-UNDO
    FIELD dat   AS DATE FORMAT "9999-99-99" 
    FIELD tim   AS CHARACTER 
    FIELD guard AS INTEGER 
    FIELD even  AS CHARACTER
    INDEX datTim IS PRIMARY dat tim.


DEFINE TEMP-TABLE ttSleep NO-UNDO 
    FIELD guard     AS INTEGER 
    FIELD sleepDate AS DATE FORMAT "9999-99-99"
    FIELD sleepTime AS CHARACTER 
    FIELD wakeDate  AS DATE FORMAT "9999-99-99"
    FIELD wakeTime  AS CHARACTER
    FIELD sleep     AS INTEGER.
     

DEFINE TEMP-TABLE ttGuardMinute NO-UNDO 
    FIELD guard  AS INTEGER 
    FIELD minute AS INTEGER  
    FIELD num    AS INTEGER.



DEFINE VARIABLE iMinute   AS INTEGER   NO-UNDO.    
DEFINE VARIABLE iSleeper  AS INTEGER   NO-UNDO.    
DEFINE VARIABLE cData     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTmpGuard AS INTEGER   NO-UNDO.

INPUT FROM VALUE("jensdahlin-progressabl/day4/input.txt").
REPEAT:
    IMPORT UNFORMATTED cData.
    
    CREATE ttData.
    ASSIGN 
        ttData.dat = DATE(ENTRY(1,TRIM(ENTRY(1, cData, "]"),"["), " "))
        ttData.tim = ENTRY(2,TRIM(ENTRY(1, cData, "]"),"["), " ").
    
    IF cData MATCHES "*#*" THEN DO:         
        ttData.guard = INTEGER(ENTRY(1, ENTRY(2, cData, "#"), " ")).
    END.
    ELSE DO:
        ttData.even = TRIM(ENTRY(2, cData, "]")).
    END.
        
        
END.
INPUT CLOSE.  

FOR EACH ttdata :
    IF ttData.guard > 0 THEN DO:
        CREATE ttSleep.
        ASSIGN ttSleep.guard = ttData.guard.
    END.
    IF ttdata.even = "falls asleep" THEN DO:
        
        /* Sover mer än en gång*/
        IF ttSleep.sleepTime <> "" THEN DO:
            iTmpGuard = ttSleep.guard.
            CREATE ttSleep.
            ASSIGN ttSleep.guard = iTmpGuard.
        END.        
        
        ASSIGN 
            ttSleep.sleepDate = ttData.dat
            ttSleep.sleepTime = ttData.tim.
    END.
    ELSE IF ttData.even = "wakes up" THEN DO:
        ASSIGN 
            ttSleep.wakeDate = ttData.dat
            ttSleep.wakeTime = ttData.tim.
    END.
    
    
    IF ttSleep.sleepTime <> "" AND ttSleep.wakeTime <> "" THEN DO:
        ttSleep.sleep = INTEGER(ENTRY(2, ttSleep.wakeTime,":")) - INTEGER(ENTRY(2, ttSleep.sleepTime,":")).
    END.
END. 

DEFINE VARIABLE cMinute AS CHARACTER NO-UNDO.
FOR EACH ttSleep WHERE ttSleep.sleepTime <> "":
    
    
    DO iMinute = INTEGER(ENTRY(2, ttSleep.sleepTime,":")) TO INTEGER(ENTRY(2, ttSleep.wakeTime,":")) - 1:
        FIND FIRST ttGuardMinute WHERE ttGuardMinute.guard  = ttSleep.guard 
                                   AND ttGuardMinute.minute = iMinute NO-ERROR.
        IF NOT AVAILABLE ttGuardMinute THEN DO:
            CREATE ttGuardMinute.
            ASSIGN
                ttGuardMinute.guard  = ttSleep.guard  
                ttGuardMinute.minute = iMinute.
        END.
        ttGuardMinute.num = ttGuardMinute.num + 1.
    END.         
END.


FOR EACH ttGuardMinute BY ttGuardMinute.num DESCENDING:
    MESSAGE "Factor: " ttGuardMinute.guard " x " ttGuardMinute.minute " = " ttGuardMinute.guard * ttGuardMinute.minute VIEW-AS ALERT-BOX.
    LEAVE.
END.