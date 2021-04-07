       CBL CICS('COBOL3') APOST
      *****************************************************************
      *                                                               *
      * MODULE NAME        = DFH0XODE                                 *
      *                                                               *
      * DESCRIPTIVE NAME = CICS TS  (Samples) Example Application -   *
      *                      CICS Endpoint for Order Dispatch service *
      *                                                               *
      *                                                               *
      *                                                               *
      *      Licensed Materials - Property of IBM                     *
      *                                                               *
      *      "Restricted Materials of IBM"                            *
      *                                                               *
      *      5655-Y04                                                 *
      *                                                               *
      *      (C) Copyright IBM Corp. 2004"                            *
      *                                                               *
      *                                                               *
      *                                                               *
      *                                                               *
      * STATUS = 7.2.0                                                *
      *                                                               *
      * FUNCTION =                                                    *
      *      This module is a COBOL application to implement a simple *
      *      service endpoint for the order dispatcher service        *
      *                                                               *
      *                                                               *
      *  CHANGE ACTIVITY :                                            *
      *       $SEG(DFH0XODE),COMP(SAMPLES),PROD(CICS TS ):            *
      *                                                               *
      *   PN= REASON REL YYMMDD HDXXIII : REMARKS                     *
      *   $D0= I07544 640 041129 HDIPCB  : ExampleApp: Outbound suppor*
      *                                                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DFH0XODE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
      * Common defintions                                             *
      *---------------------------------------------------------------*
      * Run time (debug) infomation for this invocation
        01  WS-HEADER.
           03 WS-EYECATCHER            PIC X(16)
                                        VALUE 'DFH0XODE------WS'.
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
           03 FILLER                   PIC X(9)  VALUE ' DFH0XODE'.
           03 EM-DETAIL                PIC X(50) VALUE SPACES.
      * Working Storage Variables
       01 WORKING-STORAGE-VARIABLES.
           COPY DFH0XCP7.
           COPY DFH0XCP8.

      *---------------------------------------------------------------*
      *****************************************************************
      *    L I N K A G E   S E C T I O N
      *****************************************************************
       LINKAGE SECTION.
       01 DFHCOMMAREA                  PIC X(23).

      *****************************************************************
      *    P R O C E D U R E S
      *****************************************************************
       PROCEDURE DIVISION.
      *---------------------------------------------------------------*
       MAINLINE SECTION.
      *---------------------------------------------------------------*
      * Common code                                                   *
      *---------------------------------------------------------------*
      * initialize working storage variables
           INITIALIZE WS-HEADER.
      * set up general variable
           MOVE EIBTRNID TO WS-TRANSID.
           MOVE EIBTRMID TO WS-TERMID.
           MOVE EIBTASKN TO WS-TASKNUM.
      *--------------------------------------------------------------*
      * Check commarea and obtain required details                    *
      *---------------------------------------------------------------*
      * If NO commarea received issue an ABEND
           IF EIBCALEN IS EQUAL TO ZERO
               MOVE ' NO COMMAREA RECEIVED' TO EM-DETAIL
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS ABEND ABCODE('EXCA') NODUMP END-EXEC
           END-IF
      * Initalize commarea return code to zero
           MOVE DFHCOMMAREA TO dispatchOrderRequest
           INITIALIZE DFHCOMMAREA
           MOVE 'Order in dispatch' TO confirmation
           MOVE dispatchOrderResponse TO DFHCOMMAREA
      * Return to caller
           EXEC CICS RETURN END-EXEC.
      *===============================================================*
      * Procedure to write error message to TD QUEUE(CSMT)            *
      *   message will include Date, Time, Program Name,              *
      *   and error details.                                          *
      *===============================================================*
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
