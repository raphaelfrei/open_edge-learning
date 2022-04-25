&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bd               PROGRESS
*/
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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tictac

/* Definitions for FRAME PG1                                            */
&Scoped-define FIELDS-IN-QUERY-PG1 tictac.ovictory tictac.oloses 
&Scoped-define ENABLED-FIELDS-IN-QUERY-PG1 tictac.ovictory tictac.oloses 
&Scoped-define ENABLED-TABLES-IN-QUERY-PG1 tictac
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-PG1 tictac
&Scoped-define QUERY-STRING-PG1 FOR EACH tictac SHARE-LOCK
&Scoped-define OPEN-QUERY-PG1 OPEN QUERY PG1 FOR EACH tictac SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-PG1 tictac
&Scoped-define FIRST-TABLE-IN-QUERY-PG1 tictac


/* Definitions for FRAME PG2                                            */
&Scoped-define FIELDS-IN-QUERY-PG2 tictac.xvictory tictac.xloses 
&Scoped-define ENABLED-FIELDS-IN-QUERY-PG2 tictac.xvictory tictac.xloses 
&Scoped-define ENABLED-TABLES-IN-QUERY-PG2 tictac
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-PG2 tictac
&Scoped-define QUERY-STRING-PG2 FOR EACH tictac SHARE-LOCK
&Scoped-define OPEN-QUERY-PG2 OPEN QUERY PG2 FOR EACH tictac SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-PG2 tictac
&Scoped-define FIRST-TABLE-IN-QUERY-PG2 tictac


/* Definitions for FRAME PG3                                            */
&Scoped-define FIELDS-IN-QUERY-PG3 tictac.totalmatches tictac.totalmoves ~
tictac.draw 
&Scoped-define ENABLED-FIELDS-IN-QUERY-PG3 tictac.totalmatches ~
tictac.totalmoves tictac.draw 
&Scoped-define ENABLED-TABLES-IN-QUERY-PG3 tictac
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-PG3 tictac
&Scoped-define QUERY-STRING-PG3 FOR EACH tictac SHARE-LOCK
&Scoped-define OPEN-QUERY-PG3 OPEN QUERY PG3 FOR EACH tictac SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-PG3 tictac
&Scoped-define FIRST-TABLE-IN-QUERY-PG3 tictac


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-1 btn-1 
&Scoped-Define DISPLAYED-OBJECTS rs-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-1 
     LABEL "&Resetar" 
     SIZE 14 BY .95.

DEFINE VARIABLE rs-1 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Total", 3,
"X's", 2,
"O's", 1
     SIZE 31 BY 1.19 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY PG1 FOR 
      tictac SCROLLING.

DEFINE QUERY PG2 FOR 
      tictac SCROLLING.

DEFINE QUERY PG3 FOR 
      tictac SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     rs-1 AT ROW 3.62 COL 2 NO-LABEL WIDGET-ID 60
     btn-1 AT ROW 10.52 COL 19 WIDGET-ID 30
     "Statistics for Tic Tac Toe" VIEW-AS TEXT
          SIZE 31 BY 1.67 AT ROW 1.24 COL 2 WIDGET-ID 2
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 32.6 BY 10.71 WIDGET-ID 100.

DEFINE FRAME PG2
     tictac.xvictory AT ROW 1.24 COL 9 COLON-ALIGNED WIDGET-ID 8
          LABEL "Win"
          VIEW-AS FILL-IN 
          SIZE 20 BY .95
     tictac.xloses AT ROW 2.43 COL 9 COLON-ALIGNED WIDGET-ID 10
          LABEL "Loses"
          VIEW-AS FILL-IN 
          SIZE 20 BY .95
    WITH 1 DOWN OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 5.05
         SIZE 31 BY 5.24
         TITLE "X's" WIDGET-ID 200.

DEFINE FRAME PG1
     tictac.ovictory AT ROW 1.24 COL 9 COLON-ALIGNED WIDGET-ID 14
          LABEL "Win"
          VIEW-AS FILL-IN 
          SIZE 20 BY .95
     tictac.oloses AT ROW 2.43 COL 9 COLON-ALIGNED WIDGET-ID 16
          LABEL "Loses"
          VIEW-AS FILL-IN 
          SIZE 20 BY .95
    WITH 1 DOWN OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 5.05
         SIZE 31 BY 5.24
         TITLE "O's" WIDGET-ID 300.

DEFINE FRAME PG3
     tictac.totalmatches AT ROW 1.24 COL 9 COLON-ALIGNED WIDGET-ID 24
          LABEL "Matches"
          VIEW-AS FILL-IN 
          SIZE 20 BY .95
     tictac.totalmoves AT ROW 2.43 COL 9 COLON-ALIGNED WIDGET-ID 26
          LABEL "Moves"
          VIEW-AS FILL-IN 
          SIZE 20 BY .95
     tictac.draw AT ROW 3.62 COL 9 COLON-ALIGNED WIDGET-ID 32
          LABEL "Draws"
          VIEW-AS FILL-IN 
          SIZE 20 BY .95
    WITH 1 DOWN OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 5.05
         SIZE 31 BY 5.24
         TITLE "Total" WIDGET-ID 400.


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
         TITLE              = "Tic Tac Toe - Statistics"
         HEIGHT             = 10.71
         WIDTH              = 32.6
         MAX-HEIGHT         = 45.86
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.86
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
/* REPARENT FRAME */
ASSIGN FRAME PG1:FRAME = FRAME fMain:HANDLE
       FRAME PG2:FRAME = FRAME fMain:HANDLE
       FRAME PG3:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME PG1
                                                                        */
/* SETTINGS FOR FILL-IN tictac.oloses IN FRAME PG1
   EXP-LABEL                                                            */
ASSIGN 
       tictac.oloses:READ-ONLY IN FRAME PG1        = TRUE.

/* SETTINGS FOR FILL-IN tictac.ovictory IN FRAME PG1
   EXP-LABEL                                                            */
ASSIGN 
       tictac.ovictory:READ-ONLY IN FRAME PG1        = TRUE.

/* SETTINGS FOR FRAME PG2
                                                                        */
/* SETTINGS FOR FILL-IN tictac.xloses IN FRAME PG2
   EXP-LABEL                                                            */
ASSIGN 
       tictac.xloses:READ-ONLY IN FRAME PG2        = TRUE.

/* SETTINGS FOR FILL-IN tictac.xvictory IN FRAME PG2
   EXP-LABEL                                                            */
ASSIGN 
       tictac.xvictory:READ-ONLY IN FRAME PG2        = TRUE.

/* SETTINGS FOR FRAME PG3
                                                                        */
/* SETTINGS FOR FILL-IN tictac.draw IN FRAME PG3
   EXP-LABEL                                                            */
ASSIGN 
       tictac.draw:READ-ONLY IN FRAME PG3        = TRUE.

/* SETTINGS FOR FILL-IN tictac.totalmatches IN FRAME PG3
   EXP-LABEL                                                            */
ASSIGN 
       tictac.totalmatches:READ-ONLY IN FRAME PG3        = TRUE.

/* SETTINGS FOR FILL-IN tictac.totalmoves IN FRAME PG3
   EXP-LABEL                                                            */
ASSIGN 
       tictac.totalmoves:READ-ONLY IN FRAME PG3        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME PG1
/* Query rebuild information for FRAME PG1
     _TblList          = "bd.tictac"
     _Query            is OPENED
*/  /* FRAME PG1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME PG2
/* Query rebuild information for FRAME PG2
     _TblList          = "bd.tictac"
     _Query            is OPENED
*/  /* FRAME PG2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME PG3
/* Query rebuild information for FRAME PG3
     _TblList          = "bd.tictac"
     _Query            is OPENED
*/  /* FRAME PG3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Tic Tac Toe - Statistics */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Tic Tac Toe - Statistics */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-1 wWin
ON CHOOSE OF btn-1 IN FRAME fMain /* Resetar */
DO:

RUN Resetar.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-1 wWin
ON VALUE-CHANGED OF rs-1 IN FRAME fMain
DO:
    HIDE FRAME PG1 NO-PAUSE.
    HIDE FRAME PG2 NO-PAUSE.
    HIDE FRAME PG3 NO-PAUSE.

    CASE INPUT FRAME {&FRAME-NAME} rs-1:
        WHEN 1 THEN DO:
            VIEW FRAME PG1.

        END.

        WHEN 2 THEN DO:
            VIEW FRAME PG2.

        END.

        WHEN 3 THEN DO:
            VIEW FRAME PG3.

        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

FIND FIRST tictac EXCLUSIVE-LOCK NO-ERROR.

RUN Filler.

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
  DISPLAY rs-1 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE rs-1 btn-1 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}

  {&OPEN-QUERY-PG1}
  GET FIRST PG1.
  IF AVAILABLE tictac THEN 
    DISPLAY tictac.ovictory tictac.oloses 
      WITH FRAME PG1 IN WINDOW wWin.
  ENABLE tictac.ovictory tictac.oloses 
      WITH FRAME PG1 IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-PG1}

  {&OPEN-QUERY-PG2}
  GET FIRST PG2.
  IF AVAILABLE tictac THEN 
    DISPLAY tictac.xvictory tictac.xloses 
      WITH FRAME PG2 IN WINDOW wWin.
  ENABLE tictac.xvictory tictac.xloses 
      WITH FRAME PG2 IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-PG2}

  {&OPEN-QUERY-PG3}
  GET FIRST PG3.
  IF AVAILABLE tictac THEN 
    DISPLAY tictac.totalmatches tictac.totalmoves tictac.draw 
      WITH FRAME PG3 IN WINDOW wWin.
  ENABLE tictac.totalmatches tictac.totalmoves tictac.draw 
      WITH FRAME PG3 IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-PG3}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Filler wWin 
PROCEDURE Filler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:

    IF NOT AVAIL tictac THEN DO:
        CREATE tictac.
            ASSIGN tictac.xvictory = 0
                tictac.xloses = 0
                tictac.xmoves = 0
                tictac.ovictory = 0
                tictac.oloses = 0
                tictac.omoves = 0
                tictac.totalmoves = 0
                tictac.totalmatches = 0
                tictac.draw = 0.
    END.

    /*IF AVAILABLE tictac THEN
        fll-1:SCREEN-VALUE = STRING(tictac.xvictory).
        fll-2:SCREEN-VALUE = STRING(tictac.xloses).
        fll-3:SCREEN-VALUE = STRING(tictac.xmoves).

        fll-4:SCREEN-VALUE = STRING(tictac.ovictory).
        fll-5:SCREEN-VALUE = STRING(tictac.oloses).
        fll-6:SCREEN-VALUE = STRING(tictac.omoves).

        fll-7:SCREEN-VALUE = STRING(tictac.totalmatches).
        fll-8:SCREEN-VALUE = STRING(tictac.totalmoves).*/

    /*IF AVAILABLE tictac THEN DO:
        DISPLAY tictac.xvictory
                tictac.xloses
                tictac.xmoves
                tictac.ovictory
                tictac.oloses
                tictac.omoves
                tictac.totalmatches
                tictac.totalmoves
                WITH FRAME {&FRAME-NAME}.
    END.                                 */
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resetar wWin 
PROCEDURE Resetar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
MESSAGE "Você realmente deseja resetar?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    TITLE "Resetar estatísticas" UPDATE lChoice AS LOGICAL.

CASE lChoice:
    WHEN TRUE THEN DO:
        ASSIGN tictac.xvictory = 0
            tictac.xloses = 0
            tictac.xmoves = 0
            tictac.ovictory = 0
            tictac.oloses = 0
            tictac.omoves = 0
            tictac.totalmoves = 0
            tictac.totalmatches = 0
            tictac.draw = 0.  
    END.
    WHEN FALSE THEN DO:
        MESSAGE "Operação cancelada."
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
            TITLE "Operação cancelada".
        RETURN NO-APPLY.
    END.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

