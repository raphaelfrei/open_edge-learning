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
&Scoped-Define ENABLED-OBJECTS fll3 fll1 btn1 fll2 btn2 
&Scoped-Define DISPLAYED-OBJECTS fll3 fll1 fll2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn1 
     LABEL "Encriptar" 
     SIZE 16 BY .95.

DEFINE BUTTON btn2 
     LABEL "Desencriptar" 
     SIZE 16 BY .95.

DEFINE VARIABLE fll1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 78 BY .95 NO-UNDO.

DEFINE VARIABLE fll2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 78 BY .95 NO-UNDO.

DEFINE VARIABLE fll3 AS INTEGER FORMAT "9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fll3 AT ROW 2.19 COL 66 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     fll1 AT ROW 4.1 COL 2 NO-LABEL WIDGET-ID 2
     btn1 AT ROW 5.29 COL 64 WIDGET-ID 10
     fll2 AT ROW 6.95 COL 2 NO-LABEL WIDGET-ID 4
     btn2 AT ROW 8.14 COL 64 WIDGET-ID 12
     "Key:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.48 COL 68 WIDGET-ID 42
     "caesar.w [v1.02]" VIEW-AS TEXT
          SIZE 39 BY .71 AT ROW 8.38 COL 2 WIDGET-ID 40
          FONT 13
     "Cifra de César" VIEW-AS TEXT
          SIZE 38 BY 1.91 AT ROW 1.24 COL 2 WIDGET-ID 36
     "Texto para Encriptar:" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 3.38 COL 2 WIDGET-ID 6
     "Texto para Desencriptar:" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 6.24 COL 2 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 8.33 WIDGET-ID 100.


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
         TITLE              = "Cifra de César"
         HEIGHT             = 8.33
         WIDTH              = 80
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
/* SETTINGS FOR FILL-IN fll1 IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fll2 IN FRAME fMain
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Cifra de César */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Cifra de César */
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
ON CHOOSE OF btn1 IN FRAME fMain /* Encriptar */
DO:
    DO WITH FRAME {&FRAME-NAME}:

        /* Variaveis de uso no LOOP */
        DEF VAR xx AS INT NO-UNDO.

        /* A chave define quantas casas o CHAR ira mover */
        DEF VAR chave AS INT NO-UNDO.

        /* Resultado */
        DEF VAR textoEncriptado AS CHAR NO-UNDO.

        /* Variaveis de Texto */
        DEF VAR texto AS CHAR NO-UNDO.
        DEF VAR carac AS INT NO-UNDO.

        /* Faz a leitura dos valores na tela */
        texto = CAPS(STRING(fll1:SCREEN-VALUE)).
        chave = INT(fll3:SCREEN-VALUE).

        /* O loop faz a leitura do char na posicao XX do texto,
        pega a posição ASCII de um unico CHAR, adiciona a chave
        na posição e transforma de volta para CHAR */
        REPEAT xx = 1 TO LENGTH(texto):
            carac = INT(ASC(SUBSTRING(texto, xx, 1))) + chave.

            IF carac < 55 OR carac > 100 THEN
                carac = 32.

            IF carac > 90 THEN DO:
                IF carac = 91 THEN
                    carac = 65.

                ELSE 
                    carac = 65 + (carac MODULO 90).
                    
            END.

            /* Usar para remover espaço em branco */
         /* IF carac = 32 THEN
                carac = 0. */

            textoEncriptado = textoEncriptado + CHR(carac).
        END.

        /* Define o resultado */
        fll2:SCREEN-VALUE = CAPS(textoEncriptado).

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn2 wWin
ON CHOOSE OF btn2 IN FRAME fMain /* Desencriptar */
DO:
    DO WITH FRAME {&FRAME-NAME}:

        /* Variaveis de uso no LOOP */
        DEF VAR xx AS INT NO-UNDO.

        /* A chave define quantas casas o CHAR ira mover */
        DEF VAR chave AS INT NO-UNDO.

        /* Resultado */
        DEF VAR textoEncriptado AS CHAR NO-UNDO.

        /* Variaveis de Texto */
        DEF VAR texto AS CHAR NO-UNDO.
        DEF VAR carac AS INT NO-UNDO.

        /* Faz a leitura dos valores na tela */
        texto = CAPS(STRING(fll2:SCREEN-VALUE)).
        chave = INT(fll3:SCREEN-VALUE).

        /* O loop faz a leitura do CHAR na posicao XX do texto,
        pega a posição ASCII de um unico CHAR, adiciona a chave
        na posição e transforma de volta para CHAR */
        REPEAT xx = 1 TO LENGTH(texto):
            carac = INT(ASC(SUBSTRING(texto, xx, 1))) - chave.

            IF carac < 55 OR carac > 120 THEN
                carac = 32.

            /* Está bugado a partir da letra R */
            IF carac < 65 AND carac <> 32 THEN DO:
                IF carac = 65 THEN
                    carac = 89.

                ELSE 
                    carac = carac + 26.
            END.

            /* Usar para remover espaço em branco */
         /* IF carac = 32 THEN
                carac = 0. */

            textoEncriptado = textoEncriptado + CHR(carac).
        END.

        /* Define o resultado */
        fll1:SCREEN-VALUE = textoEncriptado.

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
  DISPLAY fll3 fll1 fll2 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE fll3 fll1 btn1 fll2 btn2 
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

