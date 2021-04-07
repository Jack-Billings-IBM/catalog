      *****************************************************************
      *                                                               *
      * CONTROL BLOCK NAME = DFH0XCP7                                 *
      *                                                               *
      * DESCRIPTIVE NAME = CICS TS  (Samples) Example Application -   *
      *                     Copybook for example application          *
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
      *      This copy book is part of the example application and    *
      *      defines the commarea interface to the order dispatcher   *
      *      module                                                   *
      *                                                               *
      *  CHANGE ACTIVITY :                                            *
      *       $SEG(DFH0XCP7),COMP(SAMPLES),PROD(CICS TS ):            *
      *                                                               *
      *   PN= REASON REL YYMMDD HDXXIII : REMARKS                     *
      *   $D0= I07544 640 041129 HDIPCB  : ExampleApp: Outbound suppor*
      *                                                               *
      *****************************************************************
      * ++++++++++++++++++++ COMMENTED STRUCTURE +++++++++++++++++++++
      * Request language structure for operation 'dispatchOrder'
      *
      *    05 dispatchOrderRequest.
      *
      * Comments for: itemReferenceNumber
      * Soap message location = '/dispatchOrderRequest/itemReferenceNum
      * ber'
      * Schema datatype = 'short'
      * whiteSpace='collapse'
      * fractionDigits='0'
      * maxInclusive='9999'
      * minInclusive='0'
      * pattern='((\-+)?(0-9)+)'
      *      10 itemReferenceNumber           PIC 9(4) DISPLAY.
      *
      * Comments for: quantityRequired
      * Soap message location = '/dispatchOrderRequest/quantityRequired
      * '
      * Schema datatype = 'short'
      * whiteSpace='collapse'
      * fractionDigits='0'
      * maxInclusive='999'
      * minInclusive='0'
      * pattern='((\-+)?(0-9)+)'
      *      10 quantityRequired              PIC 9(3) DISPLAY.
      *
      * Comments for: customerId
      * Soap message location = '/dispatchOrderRequest/customerId'
      * Schema datatype = 'string'
      * whiteSpace='preserve'
      * length='8'
      *      10 customerId                    PIC X(8).
      *
      * Comments for: chargeDepartment
      * Soap message location = '/dispatchOrderRequest/chargeDepartment
      * '
      * Schema datatype = 'string'
      * whiteSpace='preserve'
      * length='8'
      *      10 chargeDepartment              PIC X(8).
      *
      *
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
           05 dispatchOrderRequest.
             10 itemReferenceNumber           PIC 9(4) DISPLAY.
             10 quantityRequired              PIC 9(3) DISPLAY.
             10 customerId                    PIC X(8).
             10 chargeDepartment              PIC X(8).
