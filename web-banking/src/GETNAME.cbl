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
      *  GETNAME

      * This program is part of the CICS Asynchronous API Redbooks
      * Internet banking Example

      * GETNAME - Get the customer name details from the locally
      *           optimised data store.
      * An account number is used to retrieve the full name
      * of the customer. The customer names database is hosted on
      * a different system within the same organisation. It is also
      * evolving over time.
      * The look up is normally responsive, although the service can
      * slow down during peak usage.

      *
      ******************************************************************

       IDENTIFICATION DIVISION.
        PROGRAM-ID. GETNAME.
        AUTHOR. GOHILPR.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
        WORKING-STORAGE SECTION.

      * Input record
       1 ACCOUNT-NUMBER-IN.
         2 CUST-NO-IN PIC X(4).

       1 RETURN-DATA.
         2 CUSTOMER-NAME          PIC X(65) VALUE ' '.

       1 CONTAINER-NAMES.
         2 INPUT-CONTAINER    PIC X(16) VALUE 'INPUTCONTAINER  '.
         2 GETNAME-CONTAINER  PIC X(16) VALUE 'GETNAMECONTAINER'.

       1 COMMAND-RESP  PIC S9(8) COMP.
       1 COMMAND-RESP2 PIC S9(8) COMP.

        LINKAGE SECTION.

       PROCEDURE DIVISION .

       MAINLINE SECTION.
       
      * Delay for 3 seconds to indicate a slow down in the database
           EXEC CICS DELAY FOR SECONDS(3) END-EXEC

      * Get the input account number

           EXEC CICS GET CONTAINER (INPUT-CONTAINER)
                           INTO    ( ACCOUNT-NUMBER-IN )
                           RESP    ( COMMAND-RESP )
                           RESP2   ( COMMAND-RESP2 )
           END-EXEC

      * "Call" the customer name retrieval service
           EVALUATE ACCOUNT-NUMBER-IN
             WHEN '0001'
               MOVE 'Pradeep Gohil'     TO CUSTOMER-NAME
             WHEN '0002'
               MOVE 'Chris Poole'       TO CUSTOMER-NAME
             WHEN '0003'
               MOVE 'Jenny He'          TO CUSTOMER-NAME
             WHEN '0004'
               MOVE 'Julian Horn'       TO CUSTOMER-NAME
             WHEN '0005'
               MOVE 'Amy Reeve'         TO CUSTOMER-NAME
             WHEN '0006'
               MOVE 'Greg Lubel'        TO CUSTOMER-NAME
             WHEN '0007'
               MOVE 'Tony Papageorgiou' TO CUSTOMER-NAME
             WHEN OTHER
               MOVE 'Simon Rachman'     TO CUSTOMER-NAME
           END-EVALUATE

      * Pass the result back to parent
           EXEC CICS PUT CONTAINER ( GETNAME-CONTAINER )
                           FROM    ( CUSTOMER-NAME )
                           RESP    ( COMMAND-RESP )
                           RESP2   ( COMMAND-RESP2 )
           END-EXEC

      * --------------------------------------------------------------
      * Finished
      * --------------------------------------------------------------
           EXEC CICS RETURN
           END-EXEC.

       END PROGRAM 'GETNAME'.