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
      *  ACCTCURR

      * This program is part of the CICS Asynchronous API Redbooks
      * Internet banking Example

      * ACCTCURR - Get the current account(s) details from the 
      *            banks own database.
      * An account number is used to retrieve the account details for
      * a customer. The customer accounts database is hosted on
      * a different system in the same organisation.
      * There may be 0 to 5 accounts per user

      *
      ******************************************************************

       IDENTIFICATION DIVISION.
        PROGRAM-ID. ACCTCURR.
        AUTHOR. GOHILPR.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
        WORKING-STORAGE SECTION.

      * Input record
       1 ACCOUNT-NUMBER-IN.
         2 CUST-NO-IN PIC X(4).

       1 RETURN-DATA.
         2 NUMBER-OF-ACCOUNTS     PIC S9(4) COMP-5 SYNC VALUE 9. 
         2 ACCOUNT-DETAILS OCCURS 5 TIMES.
           3 ACCT-NUMBER     PIC X(8) VALUE ' '.
           3 BALANCE         PIC X(8) VALUE ' '.
           3 OVERDRAFT       PIC X(8) VALUE ' '.

       1 CONTAINER-NAMES.
         2 INPUT-CONTAINER    PIC X(16) VALUE 'INPUTCONTAINER  '.
         2 ACCTCURR-CONTAINER PIC X(16) VALUE 'ACCTCURRCONT    '.

       1 COMMAND-RESP  PIC S9(8) COMP.
       1 COMMAND-RESP2 PIC S9(8) COMP.

        LINKAGE SECTION.

       PROCEDURE DIVISION .

       MAINLINE SECTION.
       
      * Delay for 3 seconds to indicate an example
      * cross system database read delay
           EXEC CICS DELAY FOR SECONDS(3) END-EXEC

      * Get the input account number

           EXEC CICS GET CONTAINER (INPUT-CONTAINER)
                           INTO    ( ACCOUNT-NUMBER-IN )
                           RESP    ( COMMAND-RESP )
                           RESP2   ( COMMAND-RESP2 )
           END-EXEC

      * "Retrieve" the account details
           IF ACCOUNT-NUMBER-IN = '0001'
           THEN
             MOVE 3 TO NUMBER-OF-ACCOUNTS

             MOVE '20140720' TO ACCT-NUMBER(1)
             MOVE '0.01    ' TO BALANCE(1)
             MOVE '0.00    ' TO OVERDRAFT(1)

             MOVE '25875343' TO ACCT-NUMBER(2)
             MOVE '45742.00' TO BALANCE(2)
             MOVE '1000.00 ' TO OVERDRAFT(2)

             MOVE '20170125' TO ACCT-NUMBER(3)
             MOVE '34533.23' TO BALANCE(3)
             MOVE '0.00    ' TO OVERDRAFT(3)

           ELSE
             MOVE 1 TO NUMBER-OF-ACCOUNTS

             MOVE '20170516' TO ACCT-NUMBER(1)
             MOVE '10.76   ' TO BALANCE(1)
             MOVE '0.00    ' TO OVERDRAFT(1)

           END-IF

      * Pass the result back to parent
           EXEC CICS PUT CONTAINER ( ACCTCURR-CONTAINER )
                           FROM    ( RETURN-DATA )
                           RESP    ( COMMAND-RESP )
                           RESP2   ( COMMAND-RESP2 )
           END-EXEC

      * --------------------------------------------------------------
      * Finished
      * --------------------------------------------------------------
           EXEC CICS RETURN
           END-EXEC.

       END PROGRAM 'ACCTCURR'.