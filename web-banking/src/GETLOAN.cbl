       PROCESS CICS,NODYNAM,NSYMBOL(NATIONAL),TRUNC(STD)
      * Licensed Materials - Property of IBM
      *
      * SAMPLE
      *
      * (c) Copyright IBM Corp. 2017 All Rights Reserved
      *
      * US Government Users Restricted Rights - Use, duplication or
      * disclosure restricted by GSA ADP Schedule Contract with IBM Corp
      *
      ******************************************************************
      *  GETLOAN

      * This program is part of the CICS Asynchronous API Redbooks
      * Internet banking Example

      * GETLOAN - Get a personalised loan rate.
      * The customers details form the input for this personalised
      * loan rate lookup service. It has to take a lot of factors into
      * consideration. Coupled with complex logic and the popularity
      * of the application, the response times can vary.

      *
      ******************************************************************

       IDENTIFICATION DIVISION.
        PROGRAM-ID. GETLOAN.
        AUTHOR. GOHILPR.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
        WORKING-STORAGE SECTION.

      * Input record
       1 ACCOUNT-NUMBER-IN.
         2 CUST-NO-IN PIC X(4).

       1 RETURN-DATA.
         2 LOAN-RATE          PIC X(8) VALUE ' '.

       1 CONTAINER-NAMES.
         2 INPUT-CONTAINER    PIC X(16) VALUE 'INPUTCONTAINER  '.
         2 GETLOAN-CONTAINER  PIC X(16) VALUE 'GETLOANCONTAINER'.

       1 COMMAND-RESP  PIC S9(8) COMP.
       1 COMMAND-RESP2 PIC S9(8) COMP.

        LINKAGE SECTION.

       PROCEDURE DIVISION .

       MAINLINE SECTION.

      * To symbolise an unreliable service, the loan rate app will
      * vary its response time.
      * Random selection of short (4 sec) or long (7 sec) response times

           IF FUNCTION CURRENT-DATE(16:1) > 5
           THEN
             DISPLAY 'Loan quote service under heavy load. ETA 7 secs.'
             EXEC CICS DELAY FOR SECONDS(7) END-EXEC
           ELSE
             DISPLAY 'Loan quote service under normal load. ETA 4 secs.'
             EXEC CICS DELAY FOR SECONDS(4) END-EXEC
           END-IF

      * Get the input account number

           EXEC CICS GET CONTAINER (INPUT-CONTAINER)
                           INTO    ( ACCOUNT-NUMBER-IN )
                           RESP    ( COMMAND-RESP )
                           RESP2   ( COMMAND-RESP2 )
           END-EXEC

      * "Calculate" the personalised loan rate
           IF ACCOUNT-NUMBER-IN = '0001'
           THEN
             MOVE '1.25' TO LOAN-RATE
           ELSE
             MOVE '7.20' TO LOAN-RATE
           END-IF

      * Pass the result back to parent
           EXEC CICS PUT CONTAINER ( GETLOAN-CONTAINER )
                           FROM    ( LOAN-RATE )
                           RESP    ( COMMAND-RESP )
                           RESP2   ( COMMAND-RESP2 )
           END-EXEC

      * --------------------------------------------------------------
      * Finished
      * --------------------------------------------------------------
           EXEC CICS RETURN
           END-EXEC.

       END PROGRAM 'GETLOAN'.