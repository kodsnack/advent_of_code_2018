
/*------------------------------------------------------------------------
    File        : day1.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Dec 03 08:06:22 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttChange NO-UNDO 
    FIELD val AS INTEGER.

DEFINE TEMP-TABLE ttFreq NO-UNDO 
    FIELD val AS INTEGER
    INDEX val val.

DEFINE VARIABLE iFrequency AS INTEGER NO-UNDO.
DEFINE VARIABLE iChange    AS INTEGER NO-UNDO.
DEFINE VARIABLE iNum       AS INTEGER NO-UNDO.
DISPLAY "Running".

INPUT FROM VALUE("jensdahlin-progressabl/day1/input.txt").
//INPUT FROM VALUE("jensdahlin-progressabl/day1/input-test5.txt").
REPEAT:
    CREATE ttChange.
    IMPORT ttChange.val.
END.
INPUT CLOSE.

/* Import bug */
FIND LAST ttChange.
DELETE ttChange.


loop:
REPEAT:
    FIND NEXT ttChange NO-ERROR.
    IF NOT AVAILABLE ttChange THEN FIND FIRST ttChange.
    
    iFrequency = iFrequency + ttChange.val.
    
    FIND FIRST ttFreq WHERE ttFreq.val = iFrequency NO-ERROR.
    IF NOT AVAILABLE ttFreq THEN DO:
        CREATE ttFreq.
        ASSIGN ttFreq.val = iFrequency.
    END.
    ELSE DO:
        MESSAGE "First dupl: " iFrequency VIEW-AS ALERT-BOX.
        LEAVE loop.
    END.
END.    
    
