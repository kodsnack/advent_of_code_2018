
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

DEFINE TEMP-TABLE ttChr NO-UNDO 
    FIELD val AS CHARACTER 
    FIELD num AS INTEGER
    INDEX val val
    INDEX num num.

DEFINE VARIABLE iDouble     AS INTEGER NO-UNDO.
DEFINE VARIABLE iTriple     AS INTEGER NO-UNDO.
DEFINE VARIABLE iTotDoubles AS INTEGER NO-UNDO.
DEFINE VARIABLE iTotTriples AS INTEGER NO-UNDO.

INPUT FROM VALUE("jensdahlin-progressabl/day2/input.txt").
REPEAT: 
    CREATE ttID.
    IMPORT ttID.
END.
INPUT CLOSE.

FOR EACH ttID:
    RUN breakdown(ttID.id, OUTPUT iDouble, OUTPUT iTriple).
    ASSIGN 
        iTotDoubles = iTotDoubles + iDouble
        iTotTriples = iTotTriples + iTriple.
END.

MESSAGE "CHKSUM:" iTotDoubles * iTotTriples VIEW-AS ALERT-BOX.

PROCEDURE breakdown:
    DEFINE INPUT  PARAMETER pcString  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER piDouble  AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER piTriple  AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE iStep AS INTEGER NO-UNDO.
    DEFINE VARIABLE cChr  AS CHARACTER NO-UNDO.
    EMPTY TEMP-TABLE ttChr.
    
    DO iStep = 1 TO LENGTH(pcString):
        cChr = SUBSTRING(pcString, iStep, 1).
        FIND FIRST ttChr WHERE  ttChr.val = cChr NO-ERROR.
        IF NOT AVAILABLE ttChr THEN DO:
            CREATE ttChr.
            ASSIGN ttChr.val = cChr.
        END.
        ASSIGN ttChr.num = ttChr.num + 1.
    END.                
    
    FIND FIRST ttChr WHERE ttChr.num = 3 NO-ERROR.
    IF AVAILABLE ttChr THEN DO:
        piTriple = piTriple + 1.
         
    END.
    
    FIND FIRST ttChr WHERE ttChr.num = 2 NO-ERROR.
    IF AVAILABLE ttChr THEN DO:
        piDouble = piDouble + 1.
    END.
     
    
END PROCEDURE.     
