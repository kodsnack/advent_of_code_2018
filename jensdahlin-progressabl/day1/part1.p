
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
DEFINE VARIABLE iFrequency AS INTEGER NO-UNDO.
DEFINE VARIABLE iChange    AS INTEGER NO-UNDO.

INPUT FROM VALUE("jensdahlin-progressabl/day1/input.txt").
REPEAT:
    IMPORT iChange.
    iFrequency = iFrequency + iChange.
END.
INPUT CLOSE.

MESSAGE iFrequency VIEW-AS ALERT-BOX.