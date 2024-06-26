       CBL CICS('COBOL3') APOST
      *****************************************************************
      *                                                               *
      *  MODULE NAME = DFH0XCMN                                       *
      *                                                               *
      *  DESCRIPTIVE NAME = CICS TS  (Samples) Example Application -  *
      *                     Catalog Manager Program                   *
      *                                                               *
      *                                                               *
      *                                                               *
      *      Licensed Materials - Property of IBM                     *
      *                                                               *
      *      "Restricted Materials of IBM"                            *
      *                                                               *
      *      5655-Y04                                                 *
      *                                                               *
      *      (C) Copyright IBM Corp. 2004, 2005"                      *
      *                                                               *
      *                                                               *
      *                                                               *
      *                                                               *
      *  STATUS = 7.2.0                                               *
      *                                                               *
      *  TRANSACTION NAME = n/a                                       *
      *                                                               *
      *  FUNCTION =                                                   *
      *  This module is the controller for the Catalog application,   *
      *  all requests pass through this module                        *
      *                                                               *
      *-------------------------------------------------------------  *
      *                                                               *
      *  ENTRY POINT = DFH0XCMN                                       *
      *                                                               *
      *-------------------------------------------------------------  *
      *                                                               *
      *  CHANGE ACTIVITY :                                            *
      *                                                               *
      *  $MOD(DFH0XCMN),COMP(PIPELINE),PROD(CICS TS ):                *
      *                                                               *
      *  PN= REASON REL YYMMDD HDXXIII : REMARKS                      *
      * $D0= I07544 640 041126 HDIPCB  : ExampleApp: Outbound support *
      * $P1= D13727 640 050217 HDIPCB  : Minor fixes to the web servic*
      *  $D0= I07544 640 040910 HDIPCB  : EXAMPLE - BASE APPLICATION  *
      *                                                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DFH0XCMN.
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
                                        VALUE 'DFH0XCMN------WS'.
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
           03 FILLER                   PIC X(9)  VALUE ' EXMPCMAN'.
           03 FILLER                   PIC X(11) VALUE ' REQUESTID='.
           03 EM-REQUEST-ID            PIC X(6)  VALUE SPACES.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-DETAIL                PIC X(50) VALUE SPACES.

      * Working variables
       01 WORKING-VARIABLES.
           03 WS-RETURN-CODE           PIC S9(8) COMP.

      * Key into the configuration file
       01 EXAMPLE-APP-CONFIG       PIC X(9)
               VALUE 'EXMP-CONF'.

      * Format of the configuration file
       01 APP-CONFIG.
           03 FILE-KEY             PIC X(9).
           03 FILLER               PIC X.
           03 DATASTORE            PIC X(4).
           03 FILLER               PIC X.
           03 DO-OUTBOUND-WS       PIC X.
           03 FILLER               PIC X.
           03 CATMAN-PROG          PIC X(8).
           03 FILLER               PIC X.
           03 DSSTUB-PROG          PIC X(8).
           03 FILLER               PIC X.
           03 DSVSAM-PROG          PIC X(8).
           03 FILLER               PIC X.
           03 ODSTUB-PROG          PIC X(8).
           03 FILLER               PIC X.
           03 ODWEBS-PROG          PIC X(8).
           03 FILLER               PIC X.
           03 STKMAN-PROG          PIC X(8).
           03 FILLER               PIC X.
           03 OUTBOUND-URL         PIC X(255).
           03 FILLER               PIC X(10).

      * Flag for Data Store program to call
       01 WS-DATASTORE-INUSE-FLAG         PIC X(4).
           88 DATASTORE-STUB                         VALUE 'STUB'.
           88 DATASTORE-VSAM                         VALUE 'VSAM'.

      * Switch For OutBound WebService on Order Dispatch
       01 WS-DISPATCHER-AS-WS-SWITCH       PIC X     VALUE 'N'.
           88 WS-DO-DISPATCHER-AS-WS                 VALUE 'Y'.

      * Program Names to LINK to
       01 WS-PROGRAM-NAMES.
           03  FILLER                      PIC X(8)  VALUE 'HHHHHHHH'.
           03  WS-DATASTORE-PROG           PIC X(8).
           03  WS-DISPATCH-PROG            PIC X(8).
           03  WS-STOCKMANAGER-PROG        PIC X(8).

      * Commarea structure for Order Dispatcher and Stock Manager Progs
       01 WS-STOCK-COMMAREA.
           COPY DFH0XCP2.

      *----------------------------------------------------------------*

      ******************************************************************
      *    L I N K A G E   S E C T I O N
      ******************************************************************
       LINKAGE SECTION.
       01 DFHCOMMAREA.
           COPY DFH0XCP3.

       01 DFHCOMMAREA2.
           COPY DFH0XCP4.  

       01 DFHCOMMAREA3.
           COPY DFH0XCP5.    

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
           INITIALIZE WS-PROGRAM-NAMES.
           INITIALIZE ERROR-MSG.

      * set up general variable
           MOVE EIBTRNID TO WS-TRANSID.
           MOVE EIBTRMID TO WS-TERMID.
           MOVE EIBTASKN TO WS-TASKNUM.

      *---------------------------------------------------------------*
      * Check commarea and obtain required details                    *
      *---------------------------------------------------------------*
      * If NO commarea received issue an ABEND
           IF EIBCALEN IS EQUAL TO ZERO
               MOVE ' NO COMMAREA RECEIVED' TO EM-DETAIL
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS ABEND ABCODE('EXCA') NODUMP END-EXEC
           END-IF

      * Initalize commarea return code to zero
           MOVE '00' TO CA-RETURN-CODE
           MOVE EIBCALEN TO WS-CALEN.

      *----------------------------------------------------------------*
      * Read in configuration file and set up program names
      *----------------------------------------------------------------*
           EXEC CICS READ FILE('EXMPCONF')
                          INTO(APP-CONFIG)
                          RIDFLD(EXAMPLE-APP-CONFIG)
                          RESP(WS-RETURN-CODE)
           END-EXEC

           IF WS-RETURN-CODE NOT EQUAL DFHRESP(NORMAL)
               MOVE '51' TO CA-RETURN-CODE
               MOVE 'APPLICATION ERROR OPENING CONFIGURATION FILE'
                   TO CA-RESPONSE-MESSAGE
               EXEC CICS RETURN END-EXEC
           END-IF

           MOVE DATASTORE TO WS-DATASTORE-INUSE-FLAG

           EVALUATE DATASTORE
               WHEN 'STUB'
                   MOVE DSSTUB-PROG TO WS-DATASTORE-PROG
               WHEN 'VSAM'
                   MOVE DSVSAM-PROG TO WS-DATASTORE-PROG
               WHEN OTHER
                   MOVE '52' TO CA-RETURN-CODE
                   MOVE 'DATASTORE TYPE INCORRECT IN CONFIGURATION FILE'
                       TO CA-RESPONSE-MESSAGE
                   EXEC CICS RETURN END-EXEC
           END-EVALUATE

           EVALUATE DO-OUTBOUND-WS
               WHEN 'Y'
                   MOVE ODWEBS-PROG TO WS-DISPATCH-PROG
               WHEN 'N'
                   MOVE ODSTUB-PROG TO WS-DISPATCH-PROG
               WHEN OTHER
                   MOVE '53' TO CA-RETURN-CODE
                   MOVE 'DISPATCHER SWITCH INCORRECT IN CONFIG FILE'
                       TO CA-RESPONSE-MESSAGE
                   EXEC CICS RETURN END-EXEC
           END-EVALUATE

           MOVE STKMAN-PROG TO WS-STOCKMANAGER-PROG

      *----------------------------------------------------------------*
      * Check which operation in being requested
      *----------------------------------------------------------------*
      * Uppercase the value passed in the Request Id field
           MOVE FUNCTION UPPER-CASE(CA-REQUEST-ID) TO CA-REQUEST-ID

           EVALUATE CA-REQUEST-ID
               WHEN '01INQC'
      *        Call routine to perform for inquire
                   PERFORM CATALOG-INQUIRE

               WHEN '01INQS'
      *        Call routine to perform for inquire for single item
                   PERFORM CATALOG-INQUIRE

               WHEN '01ORDR'
      *        Call routine to place order
                   PERFORM PLACE-ORDER

               WHEN OTHER
      *        Request is not recognised or supported
                   PERFORM REQUEST-NOT-RECOGNISED

           END-EVALUATE

      * Return to caller
           EXEC CICS RETURN END-EXEC.

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
      * Procedure to link to Datastore program to inquire              *
      *   on the catalog data                                          *
      *================================================================*
        CATALOG-INQUIRE.
           MOVE 'EXCATMAN: CATALOG-INQUIRE' TO CA-RESPONSE-MESSAGE
           EXEC CICS LINK   PROGRAM(WS-DATASTORE-PROG)
                            COMMAREA(DFHCOMMAREA)
           END-EXEC
           EXIT.

      *================================================================*
      * Procedure to link to Datastore program to place order,         *
      *   send request to dispatcher and notify stock manager          *
      *   an order has been placed                                     *
      *================================================================*
        PLACE-ORDER.
           MOVE 'EXCATMAN: PLACE-ORDER' TO CA-RESPONSE-MESSAGE
           EXEC CICS LINK PROGRAM(WS-DATASTORE-PROG)
                          COMMAREA(DFHCOMMAREA)
           END-EXEC

           IF CA-RETURN-CODE EQUAL 00
      * Link to the Order dispatch program with details
      *        Set up commarea for request
               INITIALIZE WS-STOCK-COMMAREA
               MOVE '01DSPO' TO CA-ORD-REQUEST-ID
               MOVE CA-USERID TO CA-ORD-USERID
               MOVE CA-CHARGE-DEPT TO CA-ORD-CHARGE-DEPT
               MOVE CA-ITEM-REF-NUMBER TO CA-ORD-ITEM-REF-NUMBER
               MOVE CA-QUANTITY-REQ TO CA-ORD-QUANTITY-REQ
               EXEC CICS LINK PROGRAM (WS-DISPATCH-PROG)
                              COMMAREA(WS-STOCK-COMMAREA)
               END-EXEC

               IF CA-ORD-RETURN-CODE NOT EQUAL ZERO
                   MOVE SPACES TO CA-RESPONSE-MESSAGE
                   MOVE CA-ORD-RESPONSE-MESSAGE
                         TO CA-RESPONSE-MESSAGE
               END-IF

      * Notify the stock manager program of the order details
               MOVE '01STKO' TO CA-ORD-REQUEST-ID
               EXEC CICS LINK PROGRAM (WS-STOCKMANAGER-PROG)
                              COMMAREA(WS-STOCK-COMMAREA)
               END-EXEC
           END-IF
           EXIT.

      *================================================================*
      * Procedure to handle unknown requests                           *
      *================================================================*
        REQUEST-NOT-RECOGNISED.
           MOVE '99' TO CA-RETURN-CODE

           STRING ' UNKNOWN REQUEST ID RECEIVED - ' CA-REQUEST-ID
               DELIMITED BY SIZE
               INTO EM-DETAIL
           END-STRING

           MOVE 'OPERATION UNKNOWN' TO CA-RESPONSE-MESSAGE
           EXIT.
