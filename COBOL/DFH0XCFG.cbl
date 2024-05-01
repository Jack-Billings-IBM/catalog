       CBL CICS('COBOL3') APOST
      ******************************************************************
      *                                                                *
      * MODULE NAME = DFH0XCFG                                         *
      *                                                                *
      * DESCRIPTIVE NAME = CICS TS  (Samples) Example Application -    *
      *                                       Configuration Program    *
      *                                                                *
      *                                                                *
      *                                                                *
      *     Licensed Materials - Property of IBM                       *
      *                                                                *
      *     "Restricted Materials of IBM"                              *
      *                                                                *
      *     5655-Y04                                                   *
      *                                                                *
      *     (C) Copyright IBM Corp. 2004, 2005"                        *
      *                                                                *
      *                                                                *
      *                                                                *
      *                                                                *
      * STATUS = 7.2.0                                                 *
      *                                                                *
      * TRANSACTION NAME = n/a                                         *
      *                                                                *
      * FUNCTION =                                                     *
      *      This program accesses and updates the VSAM configuration  *
      *      file used by the example application allowing uses to     *
      *      set the program names etc. used.                          *
      *                                                                *
      *                                                                *
      *                                                                *
      * ENTRY POINT = DFH0XCFG                                         *
      *                                                                *
      * CHANGE ACTIVITY :                                              *
      *      $MOD(DFH0XCFG),COMP(SAMPLES),PROD(CICS TS ):              *
      *                                                                *
      *   PN= REASON REL YYMMDD HDXXIII : REMARKS                      *
      *  $D0= I07544 640 040917 HDIPCB  : BMS MAPS FOR THE EXAMPLE APP *
      *  $D1= I07544 640 041126 HDIPCB  : ExampleApp: Outbound support *
      *  $D2= I07544 640 050114 HDIPCB  : ExampleApp CICS client code  *
      *  $D3= I07544 640 050121 HDIPCB  : ExampleApp Add sample JCL and*
      *  $D0= I07544 640 040910 HDIPCB  : EXAMPLE APP CONFIG APP       *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DFH0XCFG.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * Common defintions                                              *
      *----------------------------------------------------------------*
      * Run time (debug) infomation for this invocation
        01  WS-HEADER.
           03 WS-EYECATCHER            PIC X(16)
                                        VALUE 'DFH0XCFG------WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC 9(7).
           03 WS-CALEN                 PIC S9(4) COMP.

      * Variables for time/date processing
       01  ABS-TIME                    PIC S9(8) COMP VALUE +0.
       01  TIME1                       PIC X(8)  VALUE SPACES.
       01  DATE1                       PIC X(10) VALUE SPACES.

      * Error Message structure
       01  ERROR-MSG.
           03 EM-DATE                  PIC X(8)  VALUE SPACES.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-TIME                  PIC X(6)  VALUE SPACES.
           03 FILLER                   PIC X(9)  VALUE ' EXCONFIG'.
           03 EM-DETAIL                PIC X(50) VALUE SPACES.

      * Key into the configuration file
       01 APP-CONFIG-KEYS.
           03 APP-CONFIG-PROGS-KEY     PIC X(9)  VALUE 'EXMP-CONF'.
           03 APP-CONFIG-URL-KEY       PIC X(9)  VALUE 'OUTBNDURL'.
           03 APP-CONFIG-VSAM-KEY      PIC X(9)  VALUE 'VSAM-NAME'.
           03 APP-CONFIG-SERVER-KEY    PIC X(9)  VALUE 'WS-SERVER'.

       01 APP-EXIT-MESSAGE             PIC X(30)
                            VALUE 'EXAMPLE APPLICATION CONFIGURED'.

      * Switches
       01 SWITCHES.
            03 SEND-SWITCH             PIC X   VALUE '1'.
                88 SEND-ERASE                  VALUE '1'.
                88 SEND-DATAONLY               VALUE '2'.
                88 SEND-ALARM                  VALUE '3'.

      * Working variables
       01 WORKING-VARIABLES.
           03 WS-RESPONSE-CODE                 PIC S9(8) COMP.
           03 WS-FULL-URL.
               05 URL1                         PIC X(44).
               05 URL2                         PIC X(44).
               05 URL3                         PIC X(44).
               05 URL4                         PIC X(44).
               05 URL5                         PIC X(44).
               05 URL6                         PIC X(35).
           03 DATA-VALID-FLAG                  PIC X   VALUE '1'.
               88 DATA-VALID                           VALUE '1'.
               88 DATA-INVALID                         VALUE '2'.
           03 APP-CONFIG-NEW.
               05 APP-CONFIG-PROG-DATA-NEW.
                   07 PROGS-KEY-NEW            PIC X(9).
                   07 FILLER                   PIC X.
                   07 DATASTORE-NEW            PIC X(4).
                   07 FILLER                   PIC X.
                   07 DO-OUTBOUND-WS-NEW       PIC X.
                   07 FILLER                   PIC X.
                   07 CATMAN-PROG-NEW          PIC X(8).
                   07 FILLER                   PIC X.
                   07 DSSTUB-PROG-NEW          PIC X(8).
                   07 FILLER                   PIC X.
                   07 DSVSAM-PROG-NEW          PIC X(8).
                   07 FILLER                   PIC X.
                   07 ODSTUB-PROG-NEW          PIC X(8).
                   07 FILLER                   PIC X.
                   07 ODWEBS-PROG-NEW          PIC X(8).
                   07 FILLER                   PIC X.
                   07 STKMAN-PROG-NEW          PIC X(8).
                   07 FILLER                   PIC X(10).
               05 APP-CONFIG-URL-DATA-NEW.
                   07 URL-KEY-NEW              PIC X(9).
                   07 FILLER                   PIC X.
                   07 OUTBOUND-URL-NEW         PIC X(255).
               05 APP-CONFIG-CAT-NAME-DATA-NEW.
                   07 URL-FILE-KEY-NEW         PIC X(9).
                   07 FILLER                   PIC X.
                   07 CATALOG-FILE-NAME-NEW    PIC X(8).
                   07 FILLER                   PIC X(62).
               05 APP-CONFIG-WS-SERVERNAME-NEW.
                   07 WS-SERVER-KEY-NEW        PIC X(9).
                   07 FILLER                   PIC X.
                   07 WS-SERVER-NEW            PIC X(70).


      * Working storage copy of Commarea
      * Format of the configuration file
       01 WS-COMMAREA.
           03 APP-CONFIG.
               05 APP-CONFIG-PROG-DATA.
                   07 PROGS-KEY                PIC X(9).
                   07 FILLER                   PIC X.
                   07 DATASTORE                PIC X(4).
                   07 FILLER                   PIC X.
                   07 DO-OUTBOUND-WS           PIC X.
                   07 FILLER                   PIC X.
                   07 CATMAN-PROG              PIC X(8).
                   07 FILLER                   PIC X.
                   07 DSSTUB-PROG              PIC X(8).
                   07 FILLER                   PIC X.
                   07 DSVSAM-PROG              PIC X(8).
                   07 FILLER                   PIC X.
                   07 ODSTUB-PROG              PIC X(8).
                   07 FILLER                   PIC X.
                   07 ODWEBS-PROG              PIC X(8).
                   07 FILLER                   PIC X.
                   07 STKMAN-PROG              PIC X(8).
                   07 FILLER                   PIC X(10).
               05 APP-CONFIG-URL-DATA.
                   07 URL-KEY                  PIC X(9).
                   07 FILLER                   PIC X.
                   07 OUTBOUND-URL             PIC X(255).
               05 APP-CONFIG-CAT-NAME-DATA.
                   07 URL-FILE-KEY             PIC X(9).
                   07 FILLER                   PIC X.
                   07 CATALOG-FILE-NAME        PIC X(8).
                   07 FILLER                   PIC X(62).
               05 APP-CONFIG-WS-SERVERNAME.
                   07 WS-SERVER-KEY            PIC X(9).
                   07 FILLER                   PIC X.
                   07 WS-SERVER                PIC X(70).

       COPY DFH0XM3.
       COPY DFHAID.

      *----------------------------------------------------------------*

      ******************************************************************
      *    L I N K A G E   S E C T I O N
      ******************************************************************
       LINKAGE SECTION.
       01 DFHCOMMAREA.
           03 CONFIG-DATA                             PIC X(483).


      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       PROCEDURE DIVISION.

      *----------------------------------------------------------------*
       MAINLINE SECTION.

      *----------------------------------------------------------------*
      * Common code                                                    *
      *----------------------------------------------------------------*
      * initialize working storage variables
           INITIALIZE APP-CONFIG.
           INITIALIZE WORKING-VARIABLES.
           INITIALIZE ERROR-MSG.
           INITIALIZE EXCONFO.

      * set up general variable
           MOVE EIBTRNID TO WS-TRANSID.
           MOVE EIBTRMID TO WS-TERMID.
           MOVE EIBTASKN TO WS-TASKNUM.

           MOVE LOW-VALUE TO EXCONFO.

           IF EIBCALEN GREATER THAN ZERO
               MOVE DFHCOMMAREA TO WS-COMMAREA
           END-IF

      *----------------------------------------------------------------*
      * Check commarea and obtain required details                     *
      *----------------------------------------------------------------*

           EVALUATE TRUE
               WHEN EIBCALEN EQUAL ZERO
      *        First invocation
      *        Read current data from files
                   PERFORM READ-CONFIGURATION

      *            Populate map and send config panel
                   PERFORM POPULATE-CONFIG-DATA
                   PERFORM SEND-CONFIG-PANEL

               WHEN EIBAID EQUAL DFHCLEAR
      *        Clear key pressed - reset panel
                   PERFORM POPULATE-CONFIG-DATA
                   PERFORM SEND-CONFIG-PANEL

               WHEN EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3
      *        Attention keys - do nothing special

               WHEN EIBAID EQUAL DFHPF3 OR DFHPF12
      *        Exit application
                   PERFORM APPLICATION-EXIT

               WHEN EIBAID EQUAL DFHENTER

                   PERFORM READ-NEW-CONFIG
                   PERFORM VALIDATE-INPUT

                   IF DATA-VALID
                       PERFORM UPDATE-CONFIGURATION
                       PERFORM POPULATE-CONFIG-DATA

                       MOVE 'APPLICATION CONFIGURATION UPDATED' TO MSGO
                       SET SEND-ERASE TO TRUE
                       PERFORM SEND-CONFIG-PANEL
                   ELSE
                       SET SEND-ALARM TO TRUE
                       PERFORM SEND-CONFIG-PANEL
                   END-IF

           END-EVALUATE


      * Return to caller
           EXEC CICS RETURN TRANSID(WS-TRANSID)
                            COMMAREA(WS-COMMAREA)
           END-EXEC.

           EXIT.

       MAINLINE-EXIT.
           EXIT.
      *----------------------------------------------------------------*

      *================================================================*
      * Procedure to write error message to TD QUEUE(CSMT)             *
      *   message will include Date, Time, Program Name,               *
      *   and error details.                                           *
      *================================================================*
       WRITE-ERROR-MESSAGE.
      * Obtain and format current time and date
           EXEC CICS ASKTIME ABSTIME(ABS-TIME)
           END-EXEC
           EXEC CICS FORMATTIME ABSTIME(ABS-TIME)
                     MMDDYYYY(DATE1)
                     TIME(TIME1)
           END-EXEC
           MOVE DATE1 TO EM-DATE
           MOVE TIME1 TO EM-TIME
      * Write output message to TDQ
           EXEC CICS WRITEQ TD QUEUE('CSMT')
                     FROM(ERROR-MSG)
                     LENGTH(LENGTH OF ERROR-MSG)
           END-EXEC.
           EXIT.


      *================================================================*
      * Procedure to send the config panel BMS map                     *
      *================================================================*
        SEND-CONFIG-PANEL.
           EVALUATE TRUE
               WHEN SEND-ERASE
                   EXEC CICS SEND MAP('EXCONF')
                                  MAPSET('DFH0XS3')
                                  FROM(EXCONFO)
                                  ERASE
                   END-EXEC
               WHEN SEND-DATAONLY
                   EXEC CICS SEND MAP('EXCONF')
                                  MAPSET('DFH0XS3')
                                  FROM(EXCONFO)
                                  DATAONLY
                   END-EXEC
               WHEN SEND-ALARM
                   EXEC CICS SEND MAP('EXCONF')
                                  MAPSET('DFH0XS3')
                                  FROM(EXCONFO)
                                  DATAONLY
                                  ALARM
                   END-EXEC
           END-EVALUATE
           EXIT.


      *================================================================*
      * Procedure to read the current configuration                    *
      *================================================================*
        READ-CONFIGURATION.
      *    Read program names and options
           EXEC CICS READ FILE('BNVCONF')
                          INTO(APP-CONFIG-PROG-DATA)
                          RIDFLD(APP-CONFIG-PROGS-KEY)
                          RESP(WS-RESPONSE-CODE)
           END-EXEC
           IF WS-RESPONSE-CODE NOT EQUAL DFHRESP(NORMAL)
               MOVE 'ERROR READING FILE' TO MSGO
               SET SEND-ERASE TO TRUE
               PERFORM SEND-CONFIG-PANEL
               EXEC CICS RETURN END-EXEC
           END-IF

      *    Read URL for outbound web service call
           EXEC CICS READ FILE('BNVCONF')
                          INTO(APP-CONFIG-URL-DATA)
                          RIDFLD(APP-CONFIG-URL-KEY)
                          RESP(WS-RESPONSE-CODE)
           END-EXEC
           IF WS-RESPONSE-CODE NOT EQUAL DFHRESP(NORMAL)
               MOVE 'ERROR READING FILE' TO MSGO
               SET SEND-ERASE TO TRUE
               PERFORM SEND-CONFIG-PANEL
               EXEC CICS RETURN END-EXEC
           END-IF

      *    Read VSAM file name for catalog file
           EXEC CICS READ FILE('BNVCONF')
                          INTO(APP-CONFIG-CAT-NAME-DATA)
                          RIDFLD(APP-CONFIG-VSAM-KEY)
                          RESP(WS-RESPONSE-CODE)
           END-EXEC
           IF WS-RESPONSE-CODE NOT EQUAL DFHRESP(NORMAL)
               MOVE 'ERROR READING FILE' TO MSGO
               SET SEND-ERASE TO TRUE
               PERFORM SEND-CONFIG-PANEL
               EXEC CICS RETURN END-EXEC
           END-IF

      *    Read CICS server name and port
           EXEC CICS READ FILE('BNVCONF')
                          INTO(APP-CONFIG-WS-SERVERNAME)
                          RIDFLD(APP-CONFIG-SERVER-KEY)
                          RESP(WS-RESPONSE-CODE)
           END-EXEC
           IF WS-RESPONSE-CODE NOT EQUAL DFHRESP(NORMAL)
               MOVE 'ERROR READING FILE' TO MSGO
               SET SEND-ERASE TO TRUE
               PERFORM SEND-CONFIG-PANEL
               EXEC CICS RETURN END-EXEC
           END-IF

           EXIT.


      *================================================================*
      * Procedure to update the current configuration                  *
      *================================================================*
        UPDATE-CONFIGURATION.
      *    Read program names and options
           EXEC CICS READ FILE('BNVCONF')
                          INTO(APP-CONFIG-PROG-DATA)
                          RIDFLD(APP-CONFIG-PROGS-KEY)
                          RESP(WS-RESPONSE-CODE)
                          UPDATE
           END-EXEC
           IF WS-RESPONSE-CODE NOT EQUAL DFHRESP(NORMAL)
               MOVE 'ERROR UPDATING FILE' TO MSGO
               SET SEND-ERASE TO TRUE
               PERFORM SEND-CONFIG-PANEL
               EXEC CICS RETURN END-EXEC
           END-IF
      *    Update program names and options
           EXEC CICS REWRITE FILE('BNVCONF')
                             FROM(APP-CONFIG-PROG-DATA-NEW)
                             RESP(WS-RESPONSE-CODE)
           END-EXEC
           IF WS-RESPONSE-CODE NOT EQUAL DFHRESP(NORMAL)
               MOVE 'ERROR UPDATING FILE' TO MSGO
               SET SEND-ERASE TO TRUE
               PERFORM SEND-CONFIG-PANEL
               EXEC CICS RETURN END-EXEC
           END-IF


      *    Read URL for outbound web service call
           EXEC CICS READ FILE('BNVCONF')
                          INTO(APP-CONFIG-URL-DATA)
                          RIDFLD(APP-CONFIG-URL-KEY)
                          RESP(WS-RESPONSE-CODE)
                          UPDATE
           END-EXEC
           IF WS-RESPONSE-CODE NOT EQUAL DFHRESP(NORMAL)
               MOVE 'ERROR UPDATING FILE' TO MSGO
               SET SEND-ERASE TO TRUE
               PERFORM SEND-CONFIG-PANEL
               EXEC CICS RETURN END-EXEC
           END-IF

      *    Update URL for outbounf web service call
           EXEC CICS REWRITE FILE('BNVCONF')
                             FROM(APP-CONFIG-URL-DATA-NEW)
                             RESP(WS-RESPONSE-CODE)
           END-EXEC
           IF WS-RESPONSE-CODE NOT EQUAL DFHRESP(NORMAL)
               MOVE 'ERROR UPDATING FILE' TO MSGO
               SET SEND-ERASE TO TRUE
               PERFORM SEND-CONFIG-PANEL
               EXEC CICS RETURN END-EXEC
           END-IF

      *    Read VSAM file name for catalog file
           EXEC CICS READ FILE('BNVCONF')
                          INTO(APP-CONFIG-CAT-NAME-DATA)
                          RIDFLD(APP-CONFIG-VSAM-KEY)
                          RESP(WS-RESPONSE-CODE)
                          UPDATE
           END-EXEC
           IF WS-RESPONSE-CODE NOT EQUAL DFHRESP(NORMAL)
               MOVE 'ERROR UPDATING FILE' TO MSGO
               SET SEND-ERASE TO TRUE
               PERFORM SEND-CONFIG-PANEL
               EXEC CICS RETURN END-EXEC
           END-IF

      *    Update VSAM file name for catalog file
           EXEC CICS REWRITE FILE('BNVCONF')
                             FROM(APP-CONFIG-CAT-NAME-DATA-NEW)
                             RESP(WS-RESPONSE-CODE)
           END-EXEC
           IF WS-RESPONSE-CODE NOT EQUAL DFHRESP(NORMAL)
               MOVE 'ERROR UPDATING FILE' TO MSGO
               SET SEND-ERASE TO TRUE
               PERFORM SEND-CONFIG-PANEL
               EXEC CICS RETURN END-EXEC
           END-IF

      *    Read Server and and port
           EXEC CICS READ FILE('BNVCONF')
                          INTO(APP-CONFIG-WS-SERVERNAME)
                          RIDFLD(APP-CONFIG-SERVER-KEY)
                          RESP(WS-RESPONSE-CODE)
                          UPDATE
           END-EXEC
           IF WS-RESPONSE-CODE NOT EQUAL DFHRESP(NORMAL)
               MOVE 'ERROR UPDATING FILE' TO MSGO
               SET SEND-ERASE TO TRUE
               PERFORM SEND-CONFIG-PANEL
               EXEC CICS RETURN END-EXEC
           END-IF

      *    Update Server and port
           EXEC CICS REWRITE FILE('BNVCONF')
                             FROM(APP-CONFIG-WS-SERVERNAME-NEW)
                             RESP(WS-RESPONSE-CODE)
           END-EXEC
           IF WS-RESPONSE-CODE NOT EQUAL DFHRESP(NORMAL)
               MOVE 'ERROR UPDATING FILE' TO MSGO
               SET SEND-ERASE TO TRUE
               PERFORM SEND-CONFIG-PANEL
               EXEC CICS RETURN END-EXEC
           END-IF

           MOVE APP-CONFIG-NEW TO APP-CONFIG

           EXIT.

      *================================================================*
      * Procedure to read in the changed to the configuration          *
      *================================================================*
        READ-NEW-CONFIG.

           EXEC CICS IGNORE CONDITION MAPFAIL END-EXEC
           INITIALIZE EXCONFI
           EXEC CICS RECEIVE MAP('EXCONF')
                             MAPSET('DFH0XS3')
                             INTO(EXCONFI)
           END-EXEC

           PERFORM EXTRACT-CONFIG-DATA.


           EXIT.

      *================================================================*
      * Procedure populate the bms map with the config data            *
      *================================================================*
        POPULATE-CONFIG-DATA.

           MOVE DATASTORE TO DS-TYPEO

           IF DO-OUTBOUND-WS EQUAL 'Y'
               MOVE 'YES' TO WS-OUTBOUNDO
           ELSE
               MOVE 'NO' TO WS-OUTBOUNDO
           END-IF

           MOVE CATMAN-PROG TO CATMAN-PROGO

           MOVE DSSTUB-PROG TO DSSTUB-PROGO
           MOVE DSVSAM-PROG TO DSVSAM-PROGO
           MOVE ODSTUB-PROG TO ODSTUB-PROGO
           MOVE ODWEBS-PROG TO ODWS-PROGO
           MOVE STKMAN-PROG TO STKMAN-PROGO
           MOVE CATALOG-FILE-NAME TO VSAM-FILEO
           MOVE WS-SERVER TO WS-SERVERO
           MOVE OUTBOUND-URL TO WS-FULL-URL
               MOVE URL1 TO OUT-WS-URI1O
               MOVE URL2 TO OUT-WS-URI2O
               MOVE URL3 TO OUT-WS-URI3O
               MOVE URL4 TO OUT-WS-URI4O
               MOVE URL5 TO OUT-WS-URI5O
               MOVE URL6 TO OUT-WS-URI6O

           EXIT.

      *================================================================*
      * Procedure populate the config data from the bms map            *
      *================================================================*
        EXTRACT-CONFIG-DATA.

           MOVE APP-CONFIG TO APP-CONFIG-NEW

           IF DS-TYPEL NOT EQUAL ZERO
               MOVE FUNCTION UPPER-CASE(DS-TYPEI) TO DS-TYPEI
               MOVE DS-TYPEI TO DATASTORE-NEW
           END-IF

           IF WS-OUTBOUNDL NOT EQUAL ZERO
               MOVE FUNCTION UPPER-CASE(WS-OUTBOUNDI) TO WS-OUTBOUNDI
               IF WS-OUTBOUNDI EQUAL 'YES'
                   MOVE 'Y' TO DO-OUTBOUND-WS-NEW
               ELSE
                   MOVE 'N' TO DO-OUTBOUND-WS-NEW
               END-IF
           END-IF

           IF CATMAN-PROGL NOT EQUAL ZERO
               MOVE CATMAN-PROGI TO CATMAN-PROG-NEW
           END-IF
           IF DSSTUB-PROGL NOT EQUAL ZERO
               MOVE DSSTUB-PROGI TO DSSTUB-PROG-NEW
           END-IF
           IF DSVSAM-PROGL NOT EQUAL ZERO
               MOVE DSVSAM-PROGI TO DSVSAM-PROG-NEW
           END-IF
           IF ODSTUB-PROGL NOT EQUAL ZERO
               MOVE ODSTUB-PROGI TO ODSTUB-PROG-NEW
           END-IF
           IF ODWS-PROGL NOT EQUAL ZERO
               MOVE ODWS-PROGI TO ODWEBS-PROG-NEW
           END-IF
           IF STKMAN-PROGL NOT EQUAL ZERO
               MOVE STKMAN-PROGI TO STKMAN-PROG-NEW
           END-IF
           IF VSAM-FILEL NOT EQUAL ZERO
               MOVE VSAM-FILEI TO CATALOG-FILE-NAME-NEW
           END-IF
           IF WS-SERVERL NOT EQUAL ZERO
               MOVE WS-SERVERI TO WS-SERVER-NEW
           END-IF

           IF OUT-WS-URI1L NOT EQUAL ZERO
               STRING OUT-WS-URI1I
                      OUT-WS-URI2I
                      OUT-WS-URI3I
                      OUT-WS-URI4I
                      OUT-WS-URI5I
                      OUT-WS-URI6I
                   DELIMITED BY SIZE
                   INTO OUTBOUND-URL-NEW
               END-STRING
           END-IF

           EXIT.

      *================================================================*
      * Check values enteres are valid                                 *
      *================================================================*
        VALIDATE-INPUT.
           IF DATASTORE-NEW EQUAL 'VSAM' OR 'STUB'
               SET DATA-VALID TO TRUE
           ELSE
               SET DATA-INVALID TO TRUE
               MOVE 'PLEASE ENTER A VALID DATASTORE VALUE' TO MSGO
           END-IF

           EXIT.


      *================================================================*
      * Application Exit procedure                                     *
      *================================================================*
        APPLICATION-EXIT.
           EXEC CICS SEND TEXT FROM(APP-EXIT-MESSAGE)
                     ERASE
                     FREEKB
           END-EXEC
           EXEC CICS RETURN END-EXEC
           EXIT.