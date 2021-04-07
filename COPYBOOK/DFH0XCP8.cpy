      *****************************************************************
      *                                                               *
      * CONTROL BLOCK NAME = DFH0XCP8                                 *
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
      *       $SEG(DFH0XCP8),COMP(SAMPLES),PROD(CICS TS ):            *
      *                                                               *
      *   PN= REASON REL YYMMDD HDXXIII : REMARKS                     *
      *   $D0= I07544 640 041129 HDIPCB  : ExampleApp: Outbound suppor*
      *                                                               *
      *****************************************************************
      * ++++++++++++++++++++ COMMENTED STRUCTURE +++++++++++++++++++++
      * Response language structure for operation 'dispatchOrder'
      *
      *    05 dispatchOrderResponse.
      *
      * Comments for: confirmation
      * Soap message location = '/dispatchOrderResponse/confirmation'
      * Schema datatype = 'string'
      * whiteSpace='preserve'
      * length='20'
      *      10 confirmation                  PIC X(20).
      *
      *
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
           05 dispatchOrderResponse.
             10 confirmation                  PIC X(20).
