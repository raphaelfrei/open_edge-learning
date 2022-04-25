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
&Scoped-define BROWSE-NAME br-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES caesar_hist

/* Definitions for BROWSE br-1                                          */
&Scoped-define FIELDS-IN-QUERY-br-1 caesar_hist.enc_size ~
caesar_hist.unencrypted caesar_hist.encrypted 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-1 
&Scoped-define QUERY-STRING-br-1 FOR EACH caesar_hist NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-1 OPEN QUERY br-1 FOR EACH caesar_hist NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-1 caesar_hist
&Scoped-define FIRST-TABLE-IN-QUERY-br-1 caesar_hist


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-br-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-12 btn-8 btn-7 br-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-7 
     LABEL "&Delete" 
     SIZE 11 BY .95.

DEFINE BUTTON btn-8 
     LABEL "&Refresh" 
     SIZE 11 BY .95.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 128 BY 15.95.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-1 FOR 
      caesar_hist SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-1 wWin _STRUCTURED
  QUERY br-1 NO-LOCK DISPLAY
      caesar_hist.enc_size COLUMN-LABEL "Size" FORMAT "-9":U WIDTH 4.2
      caesar_hist.unencrypted COLUMN-LABEL "Original Text" FORMAT "x(50)":U
            WIDTH 59.4
      caesar_hist.encrypted COLUMN-LABEL "Encrypted Text" FORMAT "x(50)":U
            WIDTH 56.2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS SIZE 126 BY 15.48.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btn-8 AT ROW 1.24 COL 107 WIDGET-ID 42
     btn-7 AT ROW 1.24 COL 119 WIDGET-ID 32
     br-1 AT ROW 2.67 COL 3 WIDGET-ID 200
     "Cifra de César - Histórico" VIEW-AS TEXT
          SIZE 38 BY .95 AT ROW 1.24 COL 2 WIDGET-ID 36
     "caesar_hist.w [v1.00]" VIEW-AS TEXT
          SIZE 39 BY .71 AT ROW 18.38 COL 2 WIDGET-ID 40
     RECT-12 AT ROW 2.43 COL 2 WIDGET-ID 38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 129.6 BY 18.19 WIDGET-ID 100.


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
         TITLE              = "Cifra de César - Histórico"
         HEIGHT             = 18.19
         WIDTH              = 129.6
         MAX-HEIGHT         = 45.86
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.86
         VIRTUAL-WIDTH      = 256
         CONTROL-BOX        = no
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
/* BROWSE-TAB br-1 btn-7 fMain */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-1
/* Query rebuild information for BROWSE br-1
     _TblList          = "bd.caesar_hist"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bd.caesar_hist.enc_size
"caesar_hist.enc_size" "Size" ? "integer" ? ? ? ? ? ? no ? no no "4.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > bd.caesar_hist.unencrypted
"caesar_hist.unencrypted" "Original Text" ? "character" ? ? ? ? ? ? no ? no no "59.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > bd.caesar_hist.encrypted
"caesar_hist.encrypted" "Encrypted Text" ? "character" ? ? ? ? ? ? no ? no no "56.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Cifra de César - Histórico */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Cifra de César - Histórico */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-1
&Scoped-define SELF-NAME br-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-1 wWin
ON DEL OF br-1 IN FRAME fMain
DO:
    RUN RecDelete.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-7 wWin
ON CHOOSE OF btn-7 IN FRAME fMain /* Delete */
DO:
    RUN RecDelete.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-8 wWin
ON CHOOSE OF btn-8 IN FRAME fMain /* Refresh */
DO:
    {&OPEN-QUERY-br-1}
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
  ENABLE RECT-12 btn-8 btn-7 br-1 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RecDelete wWin 
PROCEDURE RecDelete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:

    MESSAGE "Do you want to delete the register?" 
        SKIP "This action can't be undone."
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    TITLE "Delete registry" UPDATE lChoice AS LOGICAL.

    FIND CURRENT caesar_hist EXCLUSIVE-LOCK.

    CASE lChoice:
        WHEN TRUE THEN DO:
            DELETE caesar_hist.

        END.
    
        WHEN FALSE THEN DO:
            MESSAGE "Operation skipped."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
                TITLE "Operation skipped".
            RETURN NO-APPLY.
        END. 
            
    END.
END.

{&OPEN-QUERY-br-1}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

