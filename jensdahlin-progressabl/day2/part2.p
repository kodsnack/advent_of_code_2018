
/*------------------------------------------------------------------------
    File        : part1.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Dec 03 09:06:35 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttID NO-UNDO 
    FIELD id AS CHARACTER 
    INDEX id id.

DEFINE BUFFER bbID FOR ttID.

DEFINE VARIABLE cStr AS CHARACTER NO-UNDO.

DISPLAY "Running".

INPUT FROM VALUE("jensdahlin-progressabl/day2/input.txt").
REPEAT: 
    CREATE ttID.
    IMPORT ttID.
END.
INPUT CLOSE.

/* Import bug */
FOR EACH ttId WHERE ttid.id = "":
    DELETE ttId.
END.

loop:
FOR EACH ttID:
    FOR EACH bbID:
        IF  ROWID(bbID) = ROWID(ttID) THEN NEXT.
        RUN compString(ttId.id, bbId.id, OUTPUT cStr).
        IF cStr <> "" THEN DO:
            MESSAGE "String:" REPLACE(cStr, "_", "") VIEW-AS ALERT-BOX.
            LEAVE loop.
        END.
    END.
    
END.

PROCEDURE compString:
    DEFINE INPUT  PARAMETER pcID1 AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pcID2 AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pcStr AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iPos AS INTEGER NO-UNDO.
    DEFINE VARIABLE cString AS CHARACTER NO-UNDO.
    
    cString = pcID1.
    DO iPos = 1 TO LENGTH(pcID1):
        IF SUBSTRING(pcID1, iPos, 1) <> SUBSTRING(pcID2, iPos, 1) THEN
            SUBSTRING(cString, iPos, 1) = "_". 
    END.
        
    IF NUM-ENTRIES(cString, "_") = 2 THEN DO:
        pcSTr = cString.
    END.
    
END procedure.


