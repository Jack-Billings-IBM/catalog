       CBL CICS('COBOL3') APOST
      ******************************************************************
      *                                                                *
      * MODULE NAME = DFH0XSDS                                         *
      *                                                                *
      * DESCRIPTIVE NAME = CICS TS  (Samples) Example Application -    *
      *                                       Data Store Stub          *
      *                                                                *
      *                                                                *
      *                                                                *
      *      Licensed Materials - Property of IBM                      *
      *                                                                *
      *      "Restricted Materials of IBM"                             *
      *                                                                *
      *      5655-Y04                                                  *
      *                                                                *
      *      (C) Copyright IBM Corp. 2004, 2008"                       *
      *                                                                *
      *                                                                *
      *                                                                *
      *                                                                *
      * STATUS = 7.2.0                                                 *
      *                                                                *
      * TRANSACTION NAME = n/a                                         *
      *                                                                *
      * FUNCTION =                                                     *
      *      This program is a stubbed, dummy, version of the data     *
      *      store for the example application to allow it to work     *
      *      without having to set up the VSAM file                    *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      * ENTRY POINT = DFH0XSDS                                         *
      *                                                                *
      *----------------------------------------------------------------*
      * CHANGE ACTIVITY :                                              *
      *      $MOD(DFH0XSDS),COMP(SAMPLES),PROD(CICS TS ):              *
      *                                                                *
      *   PN= REASON REL YYMMDD HDXXIII : REMARKS                      *
      *   $D0= I07544 640 040910 HDIPCB  : EXAMPLE - BASE APPLICATION  *
      *  $P1= D20555 660 080415 HDFFCMS : Typos in Web Service Example *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DFH0XSDS.
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
                                        VALUE 'DFH0XSDS------WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC 9(7).
           03 WS-CALEN                 PIC S9(4) COMP.

      * Variables for time/date processing
       01  ABS-TIME                    PIC S9(8) COMP VALUE +0.
       01  TIME1                       PIC X(8)  VALUE SPACES.
       01  DATE1                       PIC X(10) VALUE SPACES.

      * Program Names to LINK to
       01  WS-DATASTORE-PROG           PIC X(8)  VALUE SPACES.
       01  WS-DISPATCH-PROG            PIC X(8)  VALUE SPACES.
       01  WS-STOREMANAGER-PROG        PIC X(8)  VALUE SPACES.

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

      * Working Variables
       01 WORKING-VARIABLES.
           03 WS-TABLE-INDEX           PIC 9(4)  VALUE 0.
           03 WS-COMMDATA-INDEX        PIC 9(4)  VALUE 0.

      * Constants
       01 CONSTANTS.
           03 WS-NUM-ITEMS             PIC 9(4)  VALUE 15.

      * Dummy data to return on an inquire request
       01 WS-INQUIRE-RESPONSE.
           03 WS-REQUEST-ID            PIC X(6).
           03 WS-RETURN-CODE           PIC 9(2).
           03 WS-RESPONSE-MESSAGE      PIC X(79).
           03 WS-INQUIRE-REQUEST.
               05 WS-LIST-START-NUM        PIC 9(4)    VALUE 0010.
               05 WS-LAST-ITEM-NUM         PIC 9(4)    VALUE 0150.
               05 WS-ITEM-COUNT            PIC 9(3)    VALUE 015.
               05 WS-ITEM-TABLE-DATA.
                   07 WS-WST-ITEM-1.
                       09 WS-ITEM-REF          PIC 9(4)    VALUE 0010.
                       09 WS-DESCRIPTION       PIC X(40)
                       VALUE 'Ball Pens Black 24pk                    '.
                       09 WS-DEPARTMENT        PIC 9(3)    VALUE 010.
                       09 WS-COST              PIC ZZZ.99
                                                       VALUE '002.90'.
                       09 WS-IN-STOCK          PIC 9(4)    VALUE 0135.
                       09 WS-ON-ORDER          PIC 9(3)    VALUE 000.
                   07 WS-CAT-ITEM-2.
                       09 WS-ITEM-REF          PIC 9(4)    VALUE 0020.
                       09 WS-DESCRIPTION       PIC X(40)
                       VALUE 'Ball Pens Blue 24pk                     '.
                       09 WS-DEPARTMENT        PIC 9(3)    VALUE 010.
                       09 WS-COST              PIC ZZZ.99
                                                   VALUE '002.90'.
                       09 WS-IN-STOCK          PIC 9(4)    VALUE 0010.
                       09 WS-ON-ORDER          PIC 9(3)    VALUE 050.
                   07 WS-CAT-ITEM-3.
                       09 WS-ITEM-REF          PIC 9(4)    VALUE 0030.
                       09 WS-DESCRIPTION       PIC X(40)
                       VALUE 'Ball Pens Red 24pk                      '.
                       09 WS-DEPARTMENT        PIC 9(3)    VALUE 010.
                       09 WS-COST              PIC ZZZ.99
                                                   VALUE '002.90'.
                       09 WS-IN-STOCK          PIC 9(4)    VALUE 0113.
                       09 WS-ON-ORDER          PIC 9(3)    VALUE 000.
                   07 WS-CAT-ITEM-4.
                       09 WS-ITEM-REF          PIC 9(4)    VALUE 0040.
                       09 WS-DESCRIPTION       PIC X(40)
                       VALUE 'Ball Pens Green 24pk                    '.
                       09 WS-DEPARTMENT        PIC 9(3)    VALUE 010.
                       09 WS-COST              PIC ZZZ.99
                                                   VALUE '002.90'.
                       09 WS-IN-STOCK          PIC 9(4)    VALUE 0087.
                       09 WS-ON-ORDER          PIC 9(3)    VALUE 000.
                   07 WS-CAT-ITEM-5.
                       09 WS-ITEM-REF          PIC 9(4)    VALUE 0050.
                       09 WS-DESCRIPTION       PIC X(40)
                       VALUE 'Pencil with eraser 12pk                 '.
                       09 WS-DEPARTMENT        PIC 9(3)    VALUE 010.
                       09 WS-COST              PIC ZZZ.99
                                                   VALUE '001.78'.
                       09 WS-IN-STOCK          PIC 9(4)    VALUE 0093.
                       09 WS-ON-ORDER          PIC 9(3)    VALUE 000.
                   07 WS-CAT-ITEM-6.
                       09 WS-ITEM-REF          PIC 9(4)    VALUE 0060.
                       09 WS-DESCRIPTION       PIC X(40)
                       VALUE 'Highlighters Assorted 5pk               '.
                       09 WS-DEPARTMENT        PIC 9(3)    VALUE 010.
                       09 WS-COST              PIC ZZZ.99
                                                   VALUE '003.89'.
                       09 WS-IN-STOCK          PIC 9(4)    VALUE 0014.
                       09 WS-ON-ORDER          PIC 9(3)    VALUE 040.
                   07 WS-CAT-ITEM-7.
                       09 WS-ITEM-REF          PIC 9(4)    VALUE 0070.
                       09 WS-DESCRIPTION       PIC X(40)
                       VALUE 'Laser Paper 28-lb 108 Bright 500/ream   '.
                       09 WS-DEPARTMENT        PIC 9(3)    VALUE 010.
                       09 WS-COST              PIC ZZZ.99
                                                   VALUE '009.44'.
                       09 WS-IN-STOCK          PIC 9(4)    VALUE 0104.
                       09 WS-ON-ORDER          PIC 9(3)    VALUE 020.
                   07 WS-CAT-ITEM-8.
                       09 WS-ITEM-REF          PIC 9(4)    VALUE 0080.
                       09 WS-DESCRIPTION       PIC X(40)
                       VALUE 'Laser Paper 28-lb 108 Bright 2500/case  '.
                       09 WS-DEPARTMENT        PIC 9(3)    VALUE 010.
                       09 WS-COST              PIC ZZZ.99
                                                   VALUE '033.54'.
                       09 WS-IN-STOCK          PIC 9(4)    VALUE 0027.
                       09 WS-ON-ORDER          PIC 9(3)    VALUE 000.
                   07 WS-CAT-ITEM-9.
                       09 WS-ITEM-REF          PIC 9(4)    VALUE 0090.
                       09 WS-DESCRIPTION       PIC X(40)
                       VALUE 'Blue Laser Paper 20lb 500/ream          '.
                       09 WS-DEPARTMENT        PIC 9(3)    VALUE 010.
                       09 WS-COST              PIC ZZZ.99
                                                   VALUE '005.35'.
                       09 WS-IN-STOCK          PIC 9(4)    VALUE 0022.
                       09 WS-ON-ORDER          PIC 9(3)    VALUE 000.
                   07 WS-CAT-ITEM-10.
                       09 WS-ITEM-REF          PIC 9(4)    VALUE 0100.
                       09 WS-DESCRIPTION       PIC X(40)
                       VALUE 'Green Laser Paper 20lb 500/ream         '.
                       09 WS-DEPARTMENT        PIC 9(3)    VALUE 010.
                       09 WS-COST              PIC ZZZ.99
                                                   VALUE '007.35'.
                       09 WS-IN-STOCK          PIC 9(4)    VALUE 0003.
                       09 WS-ON-ORDER          PIC 9(3)    VALUE 020.
                   07 WS-CAT-ITEM-11.
                       09 WS-ITEM-REF          PIC 9(4)    VALUE 0110.
                       09 WS-DESCRIPTION       PIC X(40)
                       VALUE 'IBM Network Printer 24 - Toner cart     '.
                       09 WS-DEPARTMENT        PIC 9(3)    VALUE 010.
                       09 WS-COST              PIC ZZZ.99
                                                   VALUE '169.56'.
                       09 WS-IN-STOCK          PIC 9(4)    VALUE 0012.
                       09 WS-ON-ORDER          PIC 9(3)    VALUE 000.
                   07 WS-CAT-ITEM-12.
                       09 WS-ITEM-REF          PIC 9(4)    VALUE 0120.
                       09 WS-DESCRIPTION       PIC X(40)
                       VALUE 'Standard Diary: Week to view 8 1/4x5 3/4'.
                       09 WS-DEPARTMENT        PIC 9(3)    VALUE 010.
                       09 WS-COST              PIC ZZZ.99
                                                   VALUE '025.99'.
                       09 WS-IN-STOCK          PIC 9(4)    VALUE 0007.
                       09 WS-ON-ORDER          PIC 9(3)    VALUE 000.
                   07 WS-CAT-ITEM-13.
                       09 WS-ITEM-REF          PIC 9(4)    VALUE 0130.
                       09 WS-DESCRIPTION       PIC X(40)
                       VALUE 'Wall Planner: Eraseable 36x24           '.
                       09 WS-DEPARTMENT        PIC 9(3)    VALUE 010.
                       09 WS-COST              PIC ZZZ.99
                                                   VALUE '018.85'.
                       09 WS-IN-STOCK          PIC 9(4)    VALUE 0003.
                       09 WS-ON-ORDER          PIC 9(3)    VALUE 000.
                   07 WS-CAT-ITEM-14.
                       09 WS-ITEM-REF          PIC 9(4)    VALUE 0140.
                       09 WS-DESCRIPTION       PIC X(40)
                       VALUE '70 Sheet Hard Back wire bound notepad   '.
                       09 WS-DEPARTMENT        PIC 9(3)    VALUE 010.
                       09 WS-COST              PIC ZZZ.99
                                                   VALUE '005.89'.
                       09 WS-IN-STOCK          PIC 9(4)    VALUE 0084.
                       09 WS-ON-ORDER          PIC 9(3)    VALUE 000.
                   07 WS-CAT-ITEM-15.
                       09 WS-ITEM-REF          PIC 9(4)    VALUE 0150.
                       09 WS-DESCRIPTION       PIC X(40)
                       VALUE 'Sticky Notes 3x3 Assorted Colors 5pk    '.
                       09 WS-DEPARTMENT        PIC 9(3)    VALUE 010.
                       09 WS-COST              PIC ZZZ.99
                                                   VALUE '005.35'.
                       09 WS-IN-STOCK          PIC 9(4)    VALUE 0036.
                       09 WS-ON-ORDER          PIC 9(3)    VALUE 040.
               05 WS-ITEM-TABLE REDEFINES WS-ITEM-TABLE-DATA
                                OCCURS 15 TIMES.
                       07 WS-CAT-ITEM.
                           09 WS-ITEM-REF-NUM      PIC 9(4).
                           09 WS-DESCRIPTION       PIC X(40).
                           09 WS-DEPARTMENT        PIC 9(3).
                           09 WS-COST              PIC ZZZ.99.
                           09 WS-IN-STOCK          PIC 9(4).
                           09 WS-ON-ORDER          PIC 9(3).

      *----------------------------------------------------------------*

      ******************************************************************
      *    L I N K A G E   S E C T I O N
      ******************************************************************
       LINKAGE SECTION.
       01 DFHCOMMAREA.
           COPY DFH0XCP1.

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

      * initalize commarea return code to zero
           MOVE '00' TO CA-RETURN-CODE
           MOVE EIBCALEN TO WS-CALEN.

      *----------------------------------------------------------------*
      * Check which operation in being requested
      *----------------------------------------------------------------*
      * Uppercase the value passed in the Request Id field
           MOVE FUNCTION UPPER-CASE(CA-REQUEST-ID) TO CA-REQUEST-ID

           EVALUATE CA-REQUEST-ID
               WHEN '01INQC'
      *        Call routine to read catalog for inquire
                   PERFORM CATALOG-INQUIRE

               WHEN '01ORDR'
      *        Call routine to place order
                   PERFORM PLACE-ORDER

               WHEN '01INQS'
      *        Call routine to perform for inquire for single item
                   PERFORM CATALOG-INQUIRE-SINGLE

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
           MOVE '15 ITEMS RETURNED' TO CA-RESPONSE-MESSAGE
           MOVE WS-INQUIRE-REQUEST TO CA-INQUIRE-REQUEST
           EXIT.

      *================================================================*
      * Procedure to link to Datastore program to inquire for a single *
      *   item from the catalog data                                   *
      *================================================================*
        CATALOG-INQUIRE-SINGLE.

      *    Convert item-ref into table index by dividing by 10
           DIVIDE CA-LIST-START-REF BY 10 GIVING WS-TABLE-INDEX

      *    Test Item to make sure its within the data
           IF WS-TABLE-INDEX GREATER THAN WS-NUM-ITEMS
               MOVE 20 TO CA-RETURN-CODE
               MOVE 'ITEM NOT FOUND' TO CA-RESPONSE-MESSAGE
               EXEC CICS RETURN END-EXEC
           END-IF

      *    Populate commarea to return single item
           MOVE CA-LIST-START-REF TO CA-LAST-ITEM-REF
           MOVE 1 TO WS-COMMDATA-INDEX
           MOVE WS-ITEM-TABLE(WS-TABLE-INDEX)
                  TO CA-CAT-ITEM(WS-COMMDATA-INDEX)

           EXIT.
        CATALOG-INQUIRE-SINGLE-END.
           EXIT.

      *================================================================*
      * Procedure to link to Datastore program to place order,         *
      *   send request to dispatcher and notify stock manager          *
      *   an order has been placed                                     *
      *================================================================*
        PLACE-ORDER.
      *                                                            $P1C
           MOVE 'ORDER SUCCESSFULLY PLACED' TO CA-RESPONSE-MESSAGE
           EXIT.

      *================================================================*
      * Procedure to handle unknown requests                           *
      *================================================================*
        REQUEST-NOT-RECOGNISED.
           MOVE '99' TO CA-RETURN-CODE
           MOVE CA-REQUEST-ID TO EM-REQUEST-ID
           MOVE ' UNKNOWN REQUEST ID RECEIVED - ' TO EM-DETAIL
           MOVE CA-REQUEST-ID TO EM-DETAIL(31:6)
           MOVE 'OPERATION UNKNOWN' TO CA-RESPONSE-MESSAGE
           EXIT.
