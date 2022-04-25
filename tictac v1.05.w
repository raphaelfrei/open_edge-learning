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
&Scoped-Define ENABLED-OBJECTS btn1 RECT-7 RECT-8 btn2 RECT-9 RECT-10 ~
RECT-11 btn3 btn10 btn11 btn4 btn5 btn6 btn7 btn8 btn9 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn1  NO-FOCUS
     LABEL "" 
     SIZE 14 BY 3.33
     FONT 30.

DEFINE BUTTON btn10 
     LABEL "&Reiniciar Jogo" 
     SIZE 42 BY .95.

DEFINE BUTTON btn11 
     LABEL "&Dados..." 
     SIZE 18 BY .95.

DEFINE BUTTON btn2  NO-FOCUS
     LABEL "" 
     SIZE 14 BY 3.33
     FONT 30.

DEFINE BUTTON btn3  NO-FOCUS
     LABEL "" 
     SIZE 14 BY 3.33
     FONT 30.

DEFINE BUTTON btn4  NO-FOCUS
     LABEL "" 
     SIZE 14 BY 3.33
     FONT 30.

DEFINE BUTTON btn5  NO-FOCUS
     LABEL "" 
     SIZE 14 BY 3.33
     FONT 30.

DEFINE BUTTON btn6  NO-FOCUS
     LABEL "" 
     SIZE 14 BY 3.33
     FONT 30.

DEFINE BUTTON btn7  NO-FOCUS
     LABEL "" 
     SIZE 14 BY 3.33
     FONT 30.

DEFINE BUTTON btn8  NO-FOCUS
     LABEL "" 
     SIZE 14 BY 3.33
     FONT 30.

DEFINE BUTTON btn9  NO-FOCUS
     LABEL "" 
     SIZE 14 BY 3.33
     FONT 30.

DEFINE VARIABLE fll10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.19
     FONT 10 NO-UNDO.

DEFINE VARIABLE fll11 AS INTEGER FORMAT "9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.19
     FONT 10 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 11.43.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 11.43.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16 BY 1.67.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 11.43.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 11.43.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 11.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btn1 AT ROW 3.38 COL 3 WIDGET-ID 42
     btn2 AT ROW 3.38 COL 17 WIDGET-ID 44
     fll10 AT ROW 1.48 COL 29 COLON-ALIGNED NO-LABEL WIDGET-ID 86 NO-TAB-STOP 
     fll11 AT ROW 1.48 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 90 NO-TAB-STOP 
     btn3 AT ROW 3.38 COL 31 WIDGET-ID 46
     btn10 AT ROW 13.38 COL 3 WIDGET-ID 92
     btn11 AT ROW 14.81 COL 28 WIDGET-ID 108
     btn4 AT ROW 6.71 COL 3 WIDGET-ID 48
     btn5 AT ROW 6.71 COL 17 WIDGET-ID 50
     btn6 AT ROW 6.71 COL 31 WIDGET-ID 52
     btn7 AT ROW 10.05 COL 3 WIDGET-ID 54
     btn8 AT ROW 10.05 COL 17 WIDGET-ID 56
     btn9 AT ROW 10.05 COL 31 WIDGET-ID 58
     "tic_game.w [v1.07]" VIEW-AS TEXT
          SIZE 23 BY .95 AT ROW 14.81 COL 2 WIDGET-ID 40
     "Tic Tac Toe" VIEW-AS TEXT
          SIZE 16 BY 1.67 AT ROW 1.24 COL 2 WIDGET-ID 36
          FONT 6
     RECT-6 AT ROW 1.24 COL 30 WIDGET-ID 104
     RECT-7 AT ROW 3.14 COL 2 WIDGET-ID 106
     RECT-8 AT ROW 3.14 COL 2 WIDGET-ID 110
     RECT-9 AT ROW 3.14 COL 2 WIDGET-ID 112
     RECT-10 AT ROW 3.14 COL 2 WIDGET-ID 114
     RECT-11 AT ROW 3.14 COL 2 WIDGET-ID 116
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 45.4 BY 15.05 WIDGET-ID 100.


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
         HEIGHT             = 15.05
         WIDTH              = 45.4
         MAX-HEIGHT         = 45.76
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.76
         VIRTUAL-WIDTH      = 256
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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
       btn1:PRIVATE-DATA IN FRAME fMain     = 
                "&1".

/* SETTINGS FOR FILL-IN fll10 IN FRAME fMain
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fll10:HIDDEN IN FRAME fMain           = TRUE
       fll10:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fll11 IN FRAME fMain
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fll11:HIDDEN IN FRAME fMain           = TRUE
       fll11:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       RECT-6:HIDDEN IN FRAME fMain           = TRUE.

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

        SELF:LABEL = player.

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
    RUN RestartGame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn11 wWin
ON CHOOSE OF btn11 IN FRAME fMain /* Dados... */
DO:
    RUN tic_statistics.w.
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

        SELF:LABEL = player.

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

        SELF:LABEL = player.

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

        SELF:LABEL = player.

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

        SELF:LABEL = player.

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

        SELF:LABEL = player.

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

        SELF:LABEL = player.

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

        SELF:LABEL = player.

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

        SELF:LABEL = player.

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

    FIND FIRST tictac EXCLUSIVE-LOCK NO-ERROR.

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
  Purpose: Read all positions in the board and check if there is a winner    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    
        /* First Row */
        IF btn1:LABEL = "X" THEN
            IF btn2:LABEL = "X" THEN
                IF btn3:LABEL = "X" THEN DO:
                    RUN XWin.
                    RETURN.
                END.

        IF btn1:LABEL = "O" THEN
            IF btn2:LABEL = "O" THEN
                IF btn3:LABEL = "O" THEN DO:
                    RUN OWin.
                    RETURN.
                END.

        /* Second Row */
        IF btn4:LABEL = "X" THEN
            IF btn5:LABEL = "X" THEN
                IF btn6:LABEL = "X" THEN DO:
                    RUN XWin.
                    RETURN.
                END.

        IF btn4:LABEL = "O" THEN
            IF btn5:LABEL = "O" THEN
                IF btn6:LABEL = "O" THEN DO:
                    RUN OWin.
                    RETURN.
                END.

        /* Third Row */
        IF btn7:LABEL = "X" THEN
            IF btn8:LABEL = "X" THEN
                IF btn9:LABEL = "X" THEN DO:
                    RUN XWin.
                    RETURN.
                END.

        IF btn7:LABEL = "O" THEN
            IF btn8:LABEL = "O" THEN
                IF btn9:LABEL = "O" THEN DO:
                    RUN OWin.
                    RETURN.
                END.

       /* First Column */
        IF btn1:LABEL = "X" THEN
            IF btn4:LABEL = "X" THEN
                IF btn7:LABEL = "X" THEN DO:
                    RUN XWin.
                    RETURN.
                END.

        IF btn1:LABEL = "O" THEN
            IF btn4:LABEL = "O" THEN
                IF btn7:LABEL = "O" THEN DO:
                    RUN OWin.
                    RETURN.
                END.

        /* Second Column */
        IF btn2:LABEL = "X" THEN
            IF btn5:LABEL = "X" THEN
                IF btn8:LABEL = "X" THEN DO:
                    RUN XWin.
                    RETURN.
                END.

        IF btn2:LABEL = "O" THEN
            IF btn5:LABEL = "O" THEN
                IF btn8:LABEL = "O" THEN DO:
                    RUN OWin.
                    RETURN.
                END.

        /* Third Column */
        IF btn3:LABEL = "X" THEN
            IF btn6:LABEL = "X" THEN
                IF btn9:LABEL = "X" THEN DO:
                    RUN XWin.
                    RETURN.
                END.

        IF btn3:LABEL = "O" THEN
            IF btn6:LABEL = "O" THEN
                IF btn9:LABEL = "O" THEN DO:
                    RUN OWin.
                    RETURN.
                END.

        /* Diagonal */
        IF btn1:LABEL = "X" THEN
            IF btn5:LABEL = "X" THEN
                IF btn9:LABEL = "X" THEN DO:
                    RUN XWin.
                    RETURN.
                END.

        IF btn1:LABEL = "O" THEN
            IF btn5:LABEL = "O" THEN
                IF btn9:LABEL = "O" THEN DO:
                    RUN OWin.
                    RETURN.
                END.

         /* Reverse Diagonal */
        IF btn3:LABEL = "X" THEN
            IF btn5:LABEL = "X" THEN
                IF btn7:LABEL = "X" THEN DO:
                    RUN XWin.
                    RETURN.
                END.

        IF btn3:LABEL = "O" THEN
            IF btn5:LABEL = "O" THEN
                IF btn7:LABEL = "O" THEN DO:
                    RUN OWin.
                    RETURN.
                END.   


         /* Draw */

         IF fll11:SCREEN-VALUE = "9" THEN DO:
            RUN Draw.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Draw wWin 
PROCEDURE Draw :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

    MESSAGE "O jogo empatou."
        VIEW-AS ALERT-BOX INFORMATION
        TITLE "EMPATE".
                
    RUN DisableAllButtons.

    IF NOT AVAIL tictac THEN DO:
        CREATE tictac.
            ASSIGN tictac.xvictory = 0
                tictac.xloses = 0
                tictac.xmoves = 0
                tictac.ovictory = 0
                tictac.oloses = 0
                tictac.omoves = 0
                tictac.totalmoves = 0
                tictac.totalmatches = 0.
    END.
    
    IF AVAILABLE tictac THEN
        ASSIGN tictac.totalmatches = tictac.totalmatches + 1
                tictac.totalmoves = tictac.totalmoves + INT(fll11:SCREEN-VALUE)
                tictac.draw = tictac.draw + 1.
    ELSE MESSAGE "Database Not Found!".
END.

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
  ENABLE btn1 RECT-7 RECT-8 btn2 RECT-9 RECT-10 RECT-11 btn3 btn10 btn11 btn4 
         btn5 btn6 btn7 btn8 btn9 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OWin wWin 
PROCEDURE OWin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

    MESSAGE "O venceu o jogo."
        VIEW-AS ALERT-BOX INFORMATION
        TITLE "VITORIA".
             
    RUN DisableAllButtons.
    
    IF AVAILABLE tictac THEN
        ASSIGN tictac.ovictory = tictac.ovictory + 1
                tictac.xloses = tictac.xloses + 1
                tictac.totalmoves = tictac.totalmoves + INT(fll11:SCREEN-VALUE)
                tictac.totalmatches = tictac.totalmatches + 1
                tictac.omoves = tictac.omoves + ((INT(fll11:SCREEN-VALUE) / 2) - 1).

    IF NOT AVAIL tictac THEN DO:
        CREATE tictac.
            ASSIGN tictac.xvictory = 0
                tictac.xloses = 0
                tictac.xmoves = 0
                tictac.ovictory = 0
                tictac.oloses = 0
                tictac.omoves = 0
                tictac.totalmoves = 0
                tictac.totalmatches = 0.
    END.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RestartGame wWin 
PROCEDURE RestartGame :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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
        btn1:LABEL = "".
        btn2:LABEL = "".
        btn3:LABEL = "".
        btn4:LABEL = "".
        btn5:LABEL = "".
        btn6:LABEL = "".
        btn7:LABEL = "".
        btn8:LABEL = "".
        btn9:LABEL = "".
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE XWin wWin 
PROCEDURE XWin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

    MESSAGE "X venceu o jogo."
        VIEW-AS ALERT-BOX INFORMATION
        TITLE "VITORIA".
                        
    RUN DisableAllButtons.
    
    IF NOT AVAIL tictac THEN DO:
        CREATE tictac.
            ASSIGN tictac.xvictory = 0
                tictac.xloses = 0
                tictac.xmoves = 0
                tictac.ovictory = 0
                tictac.oloses = 0
                tictac.omoves = 0
                tictac.totalmoves = 0
                tictac.totalmatches = 0.
    END.
    
    IF AVAILABLE tictac THEN
        ASSIGN tictac.xvictory = tictac.xvictory + 1
                tictac.oloses = tictac.oloses + 1
                tictac.totalmoves = tictac.totalmoves + INT(fll11:SCREEN-VALUE)
                tictac.totalmatches = tictac.totalmatches + 1
                tictac.xmoves = tictac.xmoves + (INT(fll11:SCREEN-VALUE) / 2).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

