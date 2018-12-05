
/*------------------------------------------------------------------------
    File        : part1.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Dec 03 10:03:10 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

DISPLAY "running".

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE iX       AS INTEGER NO-UNDO.
DEFINE VARIABLE iY       AS INTEGER NO-UNDO.
DEFINE VARIABLE iCoordID AS INTEGER NO-UNDO INIT 1.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttCoord NO-UNDO
    FIELD id  AS INTEGER 
    FIELD vX  AS INTEGER 
    FIELD vY  AS INTEGER
    FIELD num AS INTEGER
    INDEX id id
    INDEX coord vX vY
    INDEX num num.

DEFINE TEMP-TABLE ttData NO-UNDO
    FIELD id AS INTEGER
    FIELD vX AS INTEGER
    FIELD vY AS INTEGER
    FIELD fW AS INTEGER 
    FIELD fH AS INTEGER
    INDEX id id
    INDEX coord vX vY.

DEFINE TEMP-TABLE ttDataCoord NO-UNDO 
    FIELD coordID AS INTEGER 
    FIELD dataID  AS INTEGER
    INDEX dataID dataID.

DEFINE VARIABLE cData AS CHARACTER NO-UNDO.    
INPUT FROM VALUE("jensdahlin-progressabl/day3/input.txt").
//INPUT FROM VALUE("jensdahlin-progressabl/day3/test1.txt").
REPEAT:
    IMPORT UNFORMATTED cData.
    IF cData <> "" THEN DO:
        CREATE ttData.
        ASSIGN 
            ttData.id = INTEGER(TRIM(ENTRY(1, cData, "@"), "# "))
            ttData.vX = INTEGER(ENTRY(1, ENTRY(1,ENTRY(2, cData, "@"), ":")))
            ttData.vY = INTEGER(ENTRY(2, ENTRY(1,ENTRY(2, cData, "@"), ":")))
            ttData.fW = INTEGER(ENTRY(1, ENTRY(2,ENTRY(2, cData, "@"), ":"), "x"))
            ttData.fH = INTEGER(ENTRY(2, ENTRY(2,ENTRY(2, cData, "@"), ":"), "x"))
            .
        
    END.
END.
INPUT CLOSE.

FOR EACH ttData:

    DO iX = ttData.vx + 1 TO ttData.vx + ttData.fw:
        DO iY = ttData.vy + 1 TO ttData.vy + ttData.fh:
            
            FIND FIRST ttCoord WHERE ttCoord.vx = ix 
                                 AND ttCoord.vy = iy NO-ERROR.
            IF NOT AVAILABLE ttCoord THEN DO:
                CREATE ttCoord.
                ASSIGN ttCoord.vx  = ix 
                       ttCoord.vy  = iy
                       ttCoord.num = 1
                       ttCoord.id  = iCoordId
                       iCoordID    = iCoordID + 1.
            END.  
            ELSE DO:
                ASSIGN 
                    ttCoord.num  = ttCoord.num + 1.
            END.
            
            CREATE ttDataCoord.
            ASSIGN 
                ttDataCoord.coordID = ttCoord.id
                ttDataCoord.dataID  = ttData.id.
                
        END.
    END. 
END.

DEFINE VARIABLE lKrock AS LOGICAL NO-UNDO.

data:
FOR EACH ttData :
    
    lKrock = FALSE.
    FOR EACH ttDataCoord WHERE ttDataCoord.dataID = ttData.id:
        FOR EACH ttCoord WHERE ttCoord.id = ttDataCoord.coordID:
            IF ttCoord.num > 1 THEN DO:
                lKrock = TRUE.
                NEXT data.
            END.
        END.
    END.
    
    IF lKrock = FALSE THEN DO:
        MESSAGE "Not overlapping:" ttData.id VIEW-AS ALERT-BOX.
        LEAVE data.
    END.
END.
