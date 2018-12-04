
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
     

DEFINE TEMP-TABLE ttGuard NO-UNDO 
    FIELD guard AS INTEGER 
    FIELD sleep AS INTEGER.


DEFINE TEMP-TABLE ttMinute NO-UNDO 
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


FOR EACH ttSleep BREAK BY ttSleep.guard:
    IF FIRST-OF(ttSleep.guard) THEN DO:
        CREATE ttGuard.
        ASSIGN ttGuard.guard = ttSleep.guard.
    END.
    ttGuard.sleep = ttGuard.sleep + ttSleep.sleep.
END.

FOR EACH ttGuard BY ttGuard.sleep DESCENDING :
    iSleeper = ttGuard.guard.
    LEAVE.
END.

FOR EACH ttSleep WHERE ttSleep.guard = iSleeper:
    
    DO iMinute = INTEGER(ENTRY(2, ttSleep.sleepTime,":")) TO INTEGER(ENTRY(2, ttSleep.wakeTime,":")) - 1:
        FIND FIRST ttMinute WHERE ttMinute.minute = iMinute NO-ERROR.
        IF NOT AVAILABLE ttMinute THEN DO:
            CREATE ttMinute.
            ASSIGN ttMinute.minute = iMinute.
        END.
        ttMinute.num = ttMinute.num + 1.
    END.
END.


FOR EACH ttMinute BY ttMinute.num DESCENDING :
    MESSAGE "Guard: " iSleeper " Most minute: " ttMinute.minute SKIP 
        "Factor:" iSleeper * ttMinute.minute 
        VIEW-AS ALERT-BOX INFORMATION.
    LEAVE.
END.