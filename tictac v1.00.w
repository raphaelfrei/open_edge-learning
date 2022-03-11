&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn1 btn2 btn3 fll1 fll2 fll3 fll4 fll5 fll6 ~
fll7 fll8 fll9 btn4 btn5 btn6 fll10 fll11 btn7 btn8 btn9 btn10 
&Scoped-Define DISPLAYED-OBJECTS fll1 fll2 fll3 fll4 fll5 fll6 fll7 fll8 ~
fll9 fll10 fll11 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn1 
     LABEL "" 
     SIZE 14 BY 3.33.

DEFINE BUTTON btn10 
     LABEL "Reiniciar Jogo" 
     SIZE 42 BY 1.19.

DEFINE BUTTON btn2 
     LABEL "" 
     SIZE 14 BY 3.33.

DEFINE BUTTON btn3 
     LABEL "" 
     SIZE 14 BY 3.33.

DEFINE BUTTON btn4 
     LABEL "" 
     SIZE 14 BY 3.33.

DEFINE BUTTON btn5 
     LABEL "" 
     SIZE 14 BY 3.33.

DEFINE BUTTON btn6 
     LABEL "" 
     SIZE 14 BY 3.33.

DEFINE BUTTON btn7 
     LABEL "" 
     SIZE 14 BY 3.33.

DEFINE BUTTON btn8 
     LABEL "" 
     SIZE 14 BY 3.33.

DEFINE BUTTON btn9 
     LABEL "" 
     SIZE 14 BY 3.33.

DEFINE VARIABLE fll1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .95 NO-UNDO.

DEFINE VARIABLE fll10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .95 NO-UNDO.

DEFINE VARIABLE fll11 AS INTEGER FORMAT "9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .95 NO-UNDO.

DEFINE VARIABLE fll2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .95 NO-UNDO.

DEFINE VARIABLE fll3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .95 NO-UNDO.

DEFINE VARIABLE fll4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .95 NO-UNDO.

DEFINE VARIABLE fll5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .95 NO-UNDO.

DEFINE VARIABLE fll6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .95 NO-UNDO.

DEFINE VARIABLE fll7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .95 NO-UNDO.

DEFINE VARIABLE fll8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .95 NO-UNDO.

DEFINE VARIABLE fll9 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btn1 AT ROW 3.86 COL 11 WIDGET-ID 42
     btn2 AT ROW 3.86 COL 25 WIDGET-ID 44
     btn3 AT ROW 3.86 COL 39 WIDGET-ID 46
     fll1 AT ROW 3.86 COL 55 COLON-ALIGNED NO-LABEL WIDGET-ID 66
     fll2 AT ROW 3.86 COL 60 COLON-ALIGNED NO-LABEL WIDGET-ID 68
     fll3 AT ROW 3.86 COL 65 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     fll4 AT ROW 5.05 COL 55 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     fll5 AT ROW 5.05 COL 60 COLON-ALIGNED NO-LABEL WIDGET-ID 74
     fll6 AT ROW 5.05 COL 65 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     fll7 AT ROW 6.24 COL 55 COLON-ALIGNED NO-LABEL WIDGET-ID 78
     fll8 AT ROW 6.24 COL 60 COLON-ALIGNED NO-LABEL WIDGET-ID 80
     fll9 AT ROW 6.24 COL 65 COLON-ALIGNED NO-LABEL WIDGET-ID 82
     btn4 AT ROW 7.19 COL 11 WIDGET-ID 48
     btn5 AT ROW 7.19 COL 25 WIDGET-ID 50
     btn6 AT ROW 7.19 COL 39 WIDGET-ID 52
     fll10 AT ROW 7.67 COL 60 COLON-ALIGNED NO-LABEL WIDGET-ID 86
     fll11 AT ROW 8.86 COL 60 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     btn7 AT ROW 10.52 COL 11 WIDGET-ID 54
     btn8 AT ROW 10.52 COL 25 WIDGET-ID 56
     btn9 AT ROW 10.52 COL 39 WIDGET-ID 58
     btn10 AT ROW 14.1 COL 11 WIDGET-ID 92
     "tictac.w [v1.00]" VIEW-AS TEXT
          SIZE 39 BY .71 AT ROW 16.24 COL 2 WIDGET-ID 40
          FONT 13
     "Tic Tac Toe" VIEW-AS TEXT
          SIZE 38 BY 1.67 AT ROW 1.24 COL 2 WIDGET-ID 36
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79.8 BY 16.19 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Tic Tac Toe"
         HEIGHT             = 16.19
         WIDTH              = 79.8
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.2
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartWindowCues" wWin _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartWindow,ab,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
ASSIGN 
       fll1:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fll10:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fll11:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fll2:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fll3:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fll4:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fll5:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fll6:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fll7:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fll8:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fll9:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Tic Tac Toe */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Tic Tac Toe */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn1 wWin
ON CHOOSE OF btn1 IN FRAME fMain
DO:
    DO WITH FRAME {&FRAME-NAME}:
        
        /* All the buttons have the same functionality, the only thing
        that changes are the buttons that they disable and which FILL
        it fills */
        DEF VAR player AS CHAR NO-UNDO.
        RUN Define-Player(OUTPUT player).

        fll1:SCREEN-VALUE = player.

        DISABLE btn1.        

        /* Run CheckForWinners after every movement */
        RUN CheckForWinners.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn10 wWin
ON CHOOSE OF btn10 IN FRAME fMain /* Reiniciar Jogo */
DO:
    DO WITH FRAME {&FRAME-NAME}:

        /* Enable all buttons */
        ENABLE btn1.
        ENABLE btn2.
        ENABLE btn3.
        ENABLE btn4.
        ENABLE btn5.
        ENABLE btn6.
        ENABLE btn7.
        ENABLE btn8.
        ENABLE btn9.

        /* Clears last player and player count */
        fll10:SCREEN-VALUE = "".
        fll11:SCREEN-VALUE = "0".

        /* Clears the board */
        fll1:SCREEN-VALUE = "".
        fll2:SCREEN-VALUE = "".
        fll3:SCREEN-VALUE = "".
        fll4:SCREEN-VALUE = "".
        fll5:SCREEN-VALUE = "".
        fll6:SCREEN-VALUE = "".
        fll7:SCREEN-VALUE = "".
        fll8:SCREEN-VALUE = "".
        fll9:SCREEN-VALUE = "".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn2 wWin
ON CHOOSE OF btn2 IN FRAME fMain
DO:
    DO WITH FRAME {&FRAME-NAME}:
        
        /* All the buttons have the same functionality, the only thing
        that changes are the buttons that they disable and which FILL
        it fills */
        DEF VAR player AS CHAR NO-UNDO.
        RUN Define-Player(OUTPUT player).

        fll2:SCREEN-VALUE = player.

        DISABLE btn2.

        /* Run CheckForWinners after every movement */
        RUN CheckForWinners.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn3 wWin
ON CHOOSE OF btn3 IN FRAME fMain
DO:
    DO WITH FRAME {&FRAME-NAME}:
        
        /* All the buttons have the same functionality, the only thing
        that changes are the buttons that they disable and which FILL
        it fills */
        DEF VAR player AS CHAR NO-UNDO.
        RUN Define-Player(OUTPUT player).

        fll3:SCREEN-VALUE = player.

        DISABLE btn3.

        /* Run CheckForWinners after every movement */
        RUN CheckForWinners.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn4 wWin
ON CHOOSE OF btn4 IN FRAME fMain
DO:
    DO WITH FRAME {&FRAME-NAME}:
        
        /* All the buttons have the same functionality, the only thing
        that changes are the buttons that they disable and which FILL
        it fills */
        DEF VAR player AS CHAR NO-UNDO.
        RUN Define-Player(OUTPUT player).

        fll4:SCREEN-VALUE = player.

        DISABLE btn4.

        /* Run CheckForWinners after every movement */
        RUN CheckForWinners.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn5 wWin
ON CHOOSE OF btn5 IN FRAME fMain
DO:
    DO WITH FRAME {&FRAME-NAME}:
        
        /* All the buttons have the same functionality, the only thing
        that changes are the buttons that they disable and which FILL
        it fills */
        DEF VAR player AS CHAR NO-UNDO.
        RUN Define-Player(OUTPUT player).

        fll5:SCREEN-VALUE = player.

        DISABLE btn5.

        /* Run CheckForWinners after every movement */
        RUN CheckForWinners.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn6 wWin
ON CHOOSE OF btn6 IN FRAME fMain
DO:
    DO WITH FRAME {&FRAME-NAME}:
        
        /* All the buttons have the same functionality, the only thing
        that changes are the buttons that they disable and which FILL
        it fills */
        DEF VAR player AS CHAR NO-UNDO.
        RUN Define-Player(OUTPUT player).

        fll6:SCREEN-VALUE = player.

        DISABLE btn6.

        /* Run CheckForWinners after every movement */
        RUN CheckForWinners.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn7 wWin
ON CHOOSE OF btn7 IN FRAME fMain
DO:
    DO WITH FRAME {&FRAME-NAME}:
        
        /* All the buttons have the same functionality, the only thing
        that changes are the buttons that they disable and which FILL
        it fills */
        DEF VAR player AS CHAR NO-UNDO.
        RUN Define-Player(OUTPUT player).

        fll7:SCREEN-VALUE = player.

        DISABLE btn7.

        /* Run CheckForWinners after every movement */
        RUN CheckForWinners.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn8 wWin
ON CHOOSE OF btn8 IN FRAME fMain
DO:
    DO WITH FRAME {&FRAME-NAME}:
        
        /* All the buttons have the same functionality, the only thing
        that changes are the buttons that they disable and which FILL
        it fills */
        DEF VAR player AS CHAR NO-UNDO.
        RUN Define-Player(OUTPUT player).

        fll8:SCREEN-VALUE = player.

        DISABLE btn8.

        /* Run CheckForWinners after every movement */
        RUN CheckForWinners.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn9 wWin
ON CHOOSE OF btn9 IN FRAME fMain
DO:
    DO WITH FRAME {&FRAME-NAME}:
        
        /* All the buttons have the same functionality, the only thing
        that changes are the buttons that they disable and which FILL
        it fills */
        DEF VAR player AS CHAR NO-UNDO.
        RUN Define-Player(OUTPUT player).

        fll9:SCREEN-VALUE = player.

        DISABLE btn9.

        /* Run CheckForWinners after every movement */
        RUN CheckForWinners.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckForWinners wWin 
PROCEDURE CheckForWinners :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    
        /* First Row */
        IF fll1:SCREEN-VALUE = "X" THEN
            IF fll2:SCREEN-VALUE = "X" THEN
                IF fll3:SCREEN-VALUE = "X" THEN DO:
                    MESSAGE "X venceu o jogo."
                    VIEW-AS ALERT-BOX INFORMATION
                    TITLE "VITORIA".
                    RUN DisableAllButtons.
                    RETURN.
                END.

        IF fll1:SCREEN-VALUE = "O" THEN
            IF fll2:SCREEN-VALUE = "O" THEN
                IF fll3:SCREEN-VALUE = "O" THEN DO:
                    MESSAGE "O venceu o jogo."
                    VIEW-AS ALERT-BOX INFORMATION
                    TITLE "VITORIA".
                    RUN DisableAllButtons.
                    RETURN.
                END.

        /* Second Row */
        IF fll4:SCREEN-VALUE = "X" THEN
            IF fll5:SCREEN-VALUE = "X" THEN
                IF fll6:SCREEN-VALUE = "X" THEN DO:
                    MESSAGE "X venceu o jogo."
                    VIEW-AS ALERT-BOX INFORMATION
                    TITLE "VITORIA".
                    RUN DisableAllButtons.
                    RETURN.
                END.

        IF fll4:SCREEN-VALUE = "O" THEN
            IF fll5:SCREEN-VALUE = "O" THEN
                IF fll6:SCREEN-VALUE = "O" THEN DO:
                    MESSAGE "O venceu o jogo."
                    VIEW-AS ALERT-BOX INFORMATION
                    TITLE "VITORIA".
                    RUN DisableAllButtons.
                    RETURN.
                END.

        /* Third Row */
        IF fll7:SCREEN-VALUE = "X" THEN
            IF fll8:SCREEN-VALUE = "X" THEN
                IF fll9:SCREEN-VALUE = "X" THEN DO:
                    MESSAGE "X venceu o jogo."
                    VIEW-AS ALERT-BOX INFORMATION
                    TITLE "VITORIA".
                    RUN DisableAllButtons.
                    RETURN.
                END.

        IF fll7:SCREEN-VALUE = "O" THEN
            IF fll8:SCREEN-VALUE = "O" THEN
                IF fll9:SCREEN-VALUE = "O" THEN DO:
                    MESSAGE "O venceu o jogo."
                    VIEW-AS ALERT-BOX INFORMATION
                    TITLE "VITORIA".
                    RUN DisableAllButtons.
                    RETURN.
                END.

       /* First Column */
        IF fll1:SCREEN-VALUE = "X" THEN
            IF fll4:SCREEN-VALUE = "X" THEN
                IF fll7:SCREEN-VALUE = "X" THEN DO:
                    MESSAGE "X venceu o jogo."
                    VIEW-AS ALERT-BOX INFORMATION
                    TITLE "VITORIA".
                    RUN DisableAllButtons.
                    RETURN.
                END.

        IF fll1:SCREEN-VALUE = "O" THEN
            IF fll4:SCREEN-VALUE = "O" THEN
                IF fll7:SCREEN-VALUE = "O" THEN DO:
                    MESSAGE "O venceu o jogo."
                    VIEW-AS ALERT-BOX INFORMATION
                    TITLE "VITORIA".
                    RUN DisableAllButtons.
                    RETURN.
                END.

        /* Second Column */
        IF fll2:SCREEN-VALUE = "X" THEN
            IF fll5:SCREEN-VALUE = "X" THEN
                IF fll8:SCREEN-VALUE = "X" THEN DO:
                    MESSAGE "X venceu o jogo."
                    VIEW-AS ALERT-BOX INFORMATION
                    TITLE "VITORIA".
                    RUN DisableAllButtons.
                    RETURN.
                END.

        IF fll2:SCREEN-VALUE = "O" THEN
            IF fll5:SCREEN-VALUE = "O" THEN
                IF fll8:SCREEN-VALUE = "O" THEN DO:
                    MESSAGE "O venceu o jogo."
                    VIEW-AS ALERT-BOX INFORMATION
                    TITLE "VITORIA".
                    RUN DisableAllButtons.
                    RETURN.
                END.

        /* Third Column */
        IF fll3:SCREEN-VALUE = "X" THEN
            IF fll6:SCREEN-VALUE = "X" THEN
                IF fll9:SCREEN-VALUE = "X" THEN DO:
                    MESSAGE "X venceu o jogo."
                    VIEW-AS ALERT-BOX INFORMATION
                    TITLE "VITORIA".
                    RUN DisableAllButtons.
                    RETURN.
                END.

        IF fll3:SCREEN-VALUE = "O" THEN
            IF fll6:SCREEN-VALUE = "O" THEN
                IF fll9:SCREEN-VALUE = "O" THEN DO:
                    MESSAGE "O venceu o jogo."
                    VIEW-AS ALERT-BOX INFORMATION
                    TITLE "VITORIA".
                    RUN DisableAllButtons.
                    RETURN.
                END.

        /* Diagonal */
        IF fll1:SCREEN-VALUE = "X" THEN
            IF fll5:SCREEN-VALUE = "X" THEN
                IF fll9:SCREEN-VALUE = "X" THEN DO:
                    MESSAGE "X venceu o jogo."
                    VIEW-AS ALERT-BOX INFORMATION
                    TITLE "VITORIA".
                    RUN DisableAllButtons.
                    RETURN.
                END.

        IF fll1:SCREEN-VALUE = "O" THEN
            IF fll5:SCREEN-VALUE = "O" THEN
                IF fll9:SCREEN-VALUE = "O" THEN DO:
                    MESSAGE "O venceu o jogo."
                    VIEW-AS ALERT-BOX INFORMATION
                    TITLE "VITORIA".
                    RUN DisableAllButtons.
                    RETURN.
                END.

         /* Reverse Diagonal */
        IF fll3:SCREEN-VALUE = "X" THEN
            IF fll5:SCREEN-VALUE = "X" THEN
                IF fll7:SCREEN-VALUE = "X" THEN DO:
                    MESSAGE "X venceu o jogo."
                    VIEW-AS ALERT-BOX INFORMATION
                    TITLE "VITORIA".
                    RUN DisableAllButtons.
                    RETURN.
                END.

        IF fll3:SCREEN-VALUE = "O" THEN
            IF fll5:SCREEN-VALUE = "O" THEN
                IF fll7:SCREEN-VALUE = "O" THEN DO:
                    MESSAGE "O venceu o jogo."
                    VIEW-AS ALERT-BOX INFORMATION
                    TITLE "VITORIA".
                    RUN DisableAllButtons.
                    RETURN.
                END.   


         /* Draw */

         IF fll11:SCREEN-VALUE = "9" THEN DO:
            MESSAGE "O jogo empatou."
                VIEW-AS ALERT-BOX INFORMATION
                TITLE "EMPATE".
            RUN DisableAllButtons.
            RETURN.
         END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Define-Player wWin 
PROCEDURE Define-Player :
/*------------------------------------------------------------------------------
  Purpose: Defines what is the next player - X or O    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:

        DEF OUTPUT PARAMETER currentPlayer AS CHAR NO-UNDO.
        DEF VAR player AS CHAR NO-UNDO.

        DEF VAR numberOfPlays AS INT NO-UNDO.

        /* Check the last player and sets the next */    
        IF fll10:SCREEN-VALUE = "X" THEN
            player = "O".
        ELSE IF fll10:SCREEN-VALUE = "O" OR player = "" THEN
            player = "X".
    
        /* Set the variables on the screen */
        fll10:SCREEN-VALUE = STRING(player).
        currentPlayer = STRING(player).

        /* Check number of plays, because DRAW can only happen after
        9 moves and no victory */
        numberOfPlays = INT(fll11:SCREEN-VALUE) + 1.
        fll11:SCREEN-VALUE = STRING(numberOfPlays).
   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisableAllButtons wWin 
PROCEDURE DisableAllButtons :
/*------------------------------------------------------------------------------
  Purpose: Disable all buttons after victory or draw    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        DISABLE btn1.
        DISABLE btn2.
        DISABLE btn3.
        DISABLE btn4.
        DISABLE btn5.
        DISABLE btn6.
        DISABLE btn7.
        DISABLE btn8.
        DISABLE btn9.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fll1 fll2 fll3 fll4 fll5 fll6 fll7 fll8 fll9 fll10 fll11 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE btn1 btn2 btn3 fll1 fll2 fll3 fll4 fll5 fll6 fll7 fll8 fll9 btn4 btn5 
         btn6 fll10 fll11 btn7 btn8 btn9 btn10 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

