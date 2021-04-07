       CBL CICS('COBOL3') APOST
      ******************************************************************
      *                                                                *
      * MODULE NAME = DFH0XWOD                                         *
      *                                                                *
      * DESCRIPTIVE NAME = CICS TS  (Samples) Example Application -    *
      *                          Outbound web service order Dispatcher *
      *                                                                *
      *                                                                *
      *                                                                *
      *      Licensed Materials - Property of IBM                      *
      *                                                                *
      *      "Restricted Materials of IBM"                             *
      *                                                                *
      *      5655-Y04                                                  *
      *                                                                *
      *      (C) Copyright IBM Corp. 2004, 2010"                       *
      *                                                                *
      *                                                                *
      *                                                                *
      *                                                                *
      * STATUS = 7.2.0                                                 *
      *                                                                *
      * TRANSACTION NAME = n/a                                         *
      *                                                                *
      * FUNCTION =                                                     *
      *      This program is a version of the order dispatcher that    *
      *      makes an outbound web service call to an order dispatcher *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      * ENTRY POINT = DFH0XWOD                                         *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      * CHANGE ACTIVITY :                                              *
      *                                                                *
      *      $MOD(DFH0XWOD),COMP(SAMPLES),PROD(CICS TS ):              *
      *                                                                *
      *   PN= REASON REL YYMMDD HDXXIII : REMARKS                      *
      *  $01= A01906 670 100818 HDBGSLS : Migrate PM01906 from SPA R660*
      *  $D0= I07544 640 041129 HDIPCB  : ExampleApp: Outbound support *
      *  $D1= I07544 640 050204 HDIPCB  : Example App - fix config pane*
      *   $D0= I07544 640 040910 HDIPCB  : EXAMPLE - BASE APPLICATION  *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DFH0XWOD.
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
                                        VALUE 'DFH0XWOD------WS'.
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
           03 FILLER                   PIC X(9)  VALUE ' DFH0XWOD'.
           03 EM-DETAIL                PIC X(50) VALUE SPACES.
      * Key into the configurations file
       01 APP-CONFIG-CONSTANTS.
           03 APP-CONFIG-FILE-NAME     PIC X(8)  VALUE 'EXMPCONF'.
           03 APP-CONFIG-URL-KEY       PIC X(9)  VALUE 'OUTBNDURL'.

      * URL Record Structure
       01 URL-RECORD-STRUCTURE.
           03 FILLER                   PIC X(10).
           03 WS-ENDPOINT-URI          PIC X(255).

      * Working Variables
       01 WORKING-VARIABLES.
           03 WS-WEBSERVICE-NAME       PIC X(32).
           03 WS-OPERATION             PIC X(255).
           03 WS-SERVICE-CONT-NAME     PIC X(16).
           03 WS-CHANNELNAME           PIC X(16).
           03 RESP                     PIC S9(8) COMP.
           03 RESP2                    PIC S9(8) COMP.

      * WebService Message Structures
       01 WS-DISPATCH-ORDER-MESSAGES.
           COPY DFH0XCP7.
           COPY DFH0XCP8.

      *----------------------------------------------------------------*

      ******************************************************************
      *    L I N K A G E   S E C T I O N
      ******************************************************************
       LINKAGE SECTION.
       01 DFHCOMMAREA.
           COPY DFH0XCP2.

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
           INITIALIZE WS-HEADER.
           INITIALIZE URL-RECORD-STRUCTURE
           INITIALIZE WS-DISPATCH-ORDER-MESSAGES
           INITIALIZE WORKING-VARIABLES

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

      * Initialize commarea return code to zero and reset the response
           MOVE '00' TO CA-ORD-RETURN-CODE
           MOVE SPACES TO CA-ORD-RESPONSE-MESSAGE
      *---------------------------------------------------------------*
      * Read in the config file to get the url to call for the external
      * dispatcher service. Store URL in WS-ENDPOINT-URI.
      *---------------------------------------------------------------*
           EXEC CICS READ FILE(APP-CONFIG-FILE-NAME)
                          RIDFLD(APP-CONFIG-URL-KEY)
                          INTO(URL-RECORD-STRUCTURE)
                          RESP(RESP)
           END-EXEC

      * Ensure that the file read was sucessful. We cannot continue if
      * we dont know what url the service it located at
           IF RESP NOT EQUAL DFHRESP(NORMAL)
               MOVE '51' TO CA-ORD-RETURN-CODE
               MOVE 'APPLICATION ERROR OPENING CONFIGURATION FILE'
                   TO CA-ORD-RESPONSE-MESSAGE
               EXEC CICS RETURN END-EXEC
           END-IF

      *---------------------------------------------------------------*
      * Set up the data for the web service call
      *---------------------------------------------------------------*
      *    'DFHWS-DATA' is the name of the container that will store
      *    the data to make the webservice request
           MOVE 'DFHWS-DATA' TO WS-SERVICE-CONT-NAME

      *    'SERVICE-CHANNEL' is the name of the channel we will pass to
      *    the web service call
           MOVE 'SERVICE-CHANNEL' TO WS-CHANNELNAME

      *    'dispatchOrder' is the name of the WEBSERVICE resource
      *    installed in this CICS region
           MOVE 'dispatchOrder' TO WS-WEBSERVICE-NAME

      *    'dispatchOrder' is the name of the operation we are going to
      *    invoke on the remote service
           MOVE 'dispatchOrder' TO WS-OPERATION

      *    Move the data from the input commarea to the
      *    'dispatchOrderRequest' variable ready to put in a container
           MOVE CA-ORD-ITEM-REF-NUMBER
               TO itemReferenceNumber IN dispatchOrderRequest
           MOVE CA-ORD-QUANTITY-REQ
               TO quantityRequired IN dispatchOrderRequest
           MOVE CA-ORD-USERID
               TO customerId IN dispatchOrderRequest
           MOVE CA-ORD-CHARGE-DEPT
               TO chargeDepartment IN dispatchOrderRequest

      *---------------------------------------------------------------*
      * Place the request data into a container on a channel
      *---------------------------------------------------------------*
           EXEC CICS PUT CONTAINER(WS-SERVICE-CONT-NAME)
                         CHANNEL(WS-CHANNELNAME)
                         FROM(dispatchOrderRequest)
           END-EXEC

      *---------------------------------------------------------------*
      * Make the Invoke call
      *---------------------------------------------------------------*

           EXEC CICS INVOKE WEBSERVICE(WS-WEBSERVICE-NAME)
                     CHANNEL(WS-CHANNELNAME)
                     URI(WS-ENDPOINT-URI)
                     OPERATION(WS-OPERATION)
                     RESP(RESP) RESP2(RESP2)
           END-EXEC.

      * Determine URI at runtime                                   @01A
           IF RESP = DFHRESP(INVREQ) AND RESP2 = 4 THEN
             EXEC CICS INVOKE SERVICE(WS-WEBSERVICE-NAME)
                       CHANNEL(WS-CHANNELNAME)
                       OPERATION(WS-OPERATION)
                       RESP(RESP) RESP2(RESP2)
             END-EXEC
           END-IF.

      * Check the return code was normal

           EVALUATE RESP
               WHEN DFHRESP(NORMAL)
                   EXEC CICS GET CONTAINER(WS-SERVICE-CONT-NAME)
                             CHANNEL(WS-CHANNELNAME)
                             INTO(CA-ORD-RESPONSE-MESSAGE)
                   END-EXEC

               WHEN DFHRESP(INVREQ)
                   MOVE 'Error calling dispatch service - INVREQ'
                       TO CA-ORD-RESPONSE-MESSAGE
                   MOVE 30 TO CA-ORD-RETURN-CODE

               WHEN DFHRESP(NOTFND)
                   MOVE 'Error calling dispatch service - NOT FOUND'
                       TO CA-ORD-RESPONSE-MESSAGE
                   MOVE 31 TO CA-ORD-RETURN-CODE

               WHEN OTHER
                   MOVE 'Error calling dispatch service'
                       TO CA-ORD-RESPONSE-MESSAGE
                   MOVE 32 TO CA-ORD-RETURN-CODE
           END-EVALUATE.

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
