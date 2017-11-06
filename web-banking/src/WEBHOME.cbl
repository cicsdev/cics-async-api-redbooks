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
      *  WEBHOME
      *
      * Is a CICS application example that supplies data to a 
      * simulated Web banking application.

      * This program is part of the CICS Asynchronous API Redbooks
      * Internet banking Example

      *
      * This example is driven via CICS terminal.
      * A customer account number (four digits)
      * is inputed into this parent coordinating program at a terminal
      * screen after running the initiating transaction
      * 'WEBH'
      * in the form:
      * WEBH nnnn
      * eg:
      * 'WEBH 0001'
      *
      *
      ******************************************************************
      *
      * **** NOTE ****
      * This is only an example to show the asynchronous API in a simple
      * form; in contrast to calling sub programs in a sequential manner
      *
      ******************************************************************

       IDENTIFICATION DIVISION.
        PROGRAM-ID. WEBHOME.
        AUTHOR. GOHILPR.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
        WORKING-STORAGE SECTION.  

      * Input record
       1 ACCOUNT-NUMBER-IN.
         2 CUST-NO-IN             PIC X(4).

      * Output record
       1 RETURN-DATA.
         2 CUSTOMER-NAME          PIC X(65) VALUE ' '.
         2 CUSTOMER-LOAN-RATE     PIC X(8)  VALUE ' '.
         2 CUSTOMER-ACCOUNTS.
          3 CURRENT-ACCOUNTS.
           4  NUMBER-OF-ACCOUNTS  PIC S9(4) COMP-5 SYNC VALUE 9.
           4  ACCOUNT-DETAILS OCCURS 5 TIMES.
            5  ACCT-NUMBER        PIC X(8) VALUE ' '.
            5  BALANCE            PIC X(8) VALUE ' '.
            5  OVERDRAFT          PIC X(8) VALUE ' '.
          3 PARTNER-ACCOUNTS.
           4  NUMBER-OF-ACCOUNTS  PIC S9(4) COMP-5 SYNC VALUE 9.
           4  ACCOUNT-DETAILS OCCURS 5 TIMES.
            5  ACCT-NUMBER        PIC X(8) VALUE ' '.
            5  BALANCE            PIC X(8) VALUE ' '.
            5  OVERDRAFT          PIC X(8) VALUE ' '.

      * For messages printed to the terminal screen
       1 TERMINAL-STATUS.
         2 PARENT-PROGRAM         PIC X(8)  VALUE 'WEBHOME'.
         2 FILLER                 PIC X(5)  VALUE ' ACC#'.
         2 ACCOUNT-NUM            PIC X(4)  VALUE '    '.
         2 FILLER                 PIC X(9)  VALUE ' STATUS( '.
         2 CURRENT-STATUS         PIC X(8)  VALUE 'RUNNING '.
         2 FILLER                 PIC X(2)  VALUE ' )'.

      * For messages displayed to the CICS log
       1 STATUS-MSG.
         2 MSG-TIME.
           3 MSG-HOUR            PIC X(2).
           3 FILLER              PIC X(1)  VALUE ':'.
           3 MSG-MIN             PIC X(2).
           3 FILLER              PIC X(1)  VALUE '.'.
           3 MSG-SEC             PIC X(2).
           3 FILLER              PIC X(1)  VALUE SPACES.
         2 MSG-TEXT              PIC X(61) VALUE ' '.

      * Maps the terminal input to obtain the account number
       1 READ-INPUT.
         2 TRANID                PIC X(4) VALUE '    '.
         2 FILLER                PIC X(1).
         2 INPUTACCNUM           PIC X(4) VALUE '    '.
       1 READ-INPUT-LENGTH       PIC S9(4) COMP-5 SYNC VALUE 9.

       1 CONTAINER-NAMES.
         2 INPUT-CONTAINER       PIC X(16) VALUE 'INPUTCONTAINER  '.
         2 GETNAME-CONTAINER     PIC X(16) VALUE 'GETNAMECONTAINER'.
         2 ACCTCURR-CONTAINER    PIC X(16) VALUE 'ACCTCURRCONT    '.
         2 ACCTPTNR-CONTAINER    PIC X(16) VALUE 'ACCTPTNRCONT    '.
         2 GETLOAN-CONTAINER     PIC X(16) VALUE 'GETLOANCONTAINER'.
         2 ACCOUNTS-CONTAINER    PIC X(16) VALUE 'ALLCUSTACCOUNTS '.

       1 MYCHANNEL               PIC X(16) VALUE 'MYCHANNEL       '.

       1 PROGRAM-NAMES.
         2 GET-NAME              PIC X(8) VALUE 'GETNAME '.
         2 ACCTCURR              PIC X(8) VALUE 'ACCTCURR'.
         2 ACCTPTNR              PIC X(8) VALUE 'ACCTPTNR'.
         2 GETLOAN               PIC X(8) VALUE 'GETLOAN '.

       1 TRANSIDS.
         2 GET-NAME-TRAN         PIC X(4) VALUE 'GETN'.
         2 ACCTCURR-TRAN         PIC X(4) VALUE 'ACUR'.
         2 ACCTPTNR-TRAN         PIC X(4) VALUE 'PTNR'.
         2 GETLOAN-TRAN          PIC X(4) VALUE 'GETL'.

       1 CHILD-TOKENS.
         2 ANY-CHILD-TKN         PIC X(16).
         2 GET-NAME-TKN          PIC X(16).
         2 ACCTCURR-TKN          PIC X(16).
         2 ACCTPTNR-TKN          PIC X(16).
         2 GET-LOAN-TKN          PIC X(16).

       1 RETURN-CHANNELS.
         2 ANY-CHILD-CHAN        PIC X(16).
         2 GET-NAME-CHAN         PIC X(16).
         2 ACCTCURR-CHAN         PIC X(16).
         2 ACCTPTNR-CHAN         PIC X(16).
         2 GET-LOAN-CHAN         PIC X(16).

       1 CHILD-RETURN-STATUS     PIC S9(8) USAGE BINARY.
       1 CHILD-RETURN-ABCODE     PIC X(4).

       1 COMMAND-RESP            PIC S9(8) COMP.
       1 COMMAND-RESP2           PIC S9(8) COMP.

      * Record for TSQ containing timeout details for loan quote
       1 TIMEOUT-TSQ.
         2 TSQ-NAME              PIC X(8) VALUE 'LTIMEOUT'.
         2 TSQ-TIMEOUT           PIC X(8) VALUE '        '.
         2 TIMEOUT-LEN           PIC S9(4) USAGE BINARY.
       1 LOAN-RATE-TIMEOUT       PIC S9(8) USAGE BINARY VALUE 0.

       1 COUNTER                 PIC S9(4) COMP-5 SYNC VALUE 9.

        LINKAGE SECTION.

       PROCEDURE DIVISION.

       MAINLINE SECTION.
      * --------------------------------------------------------------
      * Start of the main code execution
      * --------------------------------------------------------------

      * Display a message to easily identify start of execution
           INITIALIZE STATUS-MSG
           MOVE 'Started Web banking log-on data retrieval' TO MSG-TEXT
           PERFORM PRINT-STATUS-MESSAGE

      * First step is to retrieve the account number
           PERFORM GET-INPUT-ACCOUNT-NUMBER

      * ----
      * Create the input container for children to access
      * ----
           EXEC CICS PUT CONTAINER ( INPUT-CONTAINER )
                           FROM    ( ACCOUNT-NUMBER-IN )
                           CHANNEL ( MYCHANNEL)
                           RESP    ( COMMAND-RESP )
                           RESP2   ( COMMAND-RESP2 )
           END-EXEC

           PERFORM CHECK-COMMAND

      * --------------------------------------------------------
      * Asynchronously run PNTR to get account details
      * from the partner bank
      * --------------------------------------------------------
           EXEC CICS RUN TRANSID ( ACCTPTNR-TRAN )
                         CHANNEL ( MYCHANNEL )
                         CHILD   ( ACCTPTNR-TKN )
                         RESP    ( COMMAND-RESP )
                         RESP2   ( COMMAND-RESP2 )
           END-EXEC

           PERFORM CHECK-COMMAND

      * --------------------------------------------------------
      * Asynchronously run GETN to get the customers name
      * --------------------------------------------------------
           EXEC CICS RUN TRANSID ( GET-NAME-TRAN )
                         CHANNEL ( MYCHANNEL )
                         CHILD   ( GET-NAME-TKN )
                         RESP    ( COMMAND-RESP )
                         RESP2   ( COMMAND-RESP2 )
           END-EXEC

           PERFORM CHECK-COMMAND

      * --------------------------------------------------------
      * Asynchronously run ACUR to get customers
      * current account details
      * --------------------------------------------------------
           EXEC CICS RUN TRANSID ( ACCTCURR-TRAN )
                         CHANNEL ( MYCHANNEL )
                         CHILD   ( ACCTCURR-TKN )
                         RESP    ( COMMAND-RESP )
                         RESP2   ( COMMAND-RESP2 )
           END-EXEC

           PERFORM CHECK-COMMAND

      * --------------------------------------------------------------
      * Three child tasks have been run to execute asynchronously.
      * Loop through the children to get the customer's details
      * --------------------------------------------------------------
           PERFORM 3 TIMES

             EXEC CICS FETCH ANY        ( ANY-CHILD-TKN )
                             CHANNEL    ( ANY-CHILD-CHAN )
                             COMPSTATUS ( CHILD-RETURN-STATUS )
                             ABCODE     ( CHILD-RETURN-ABCODE )
                             RESP       ( COMMAND-RESP )
                             RESP2      ( COMMAND-RESP2 )
             END-EXEC

             PERFORM CHECK-COMMAND
             PERFORM CHECK-CHILD

      *      -----  
      *      Identify which child completed and process results
      *      -----  
             EVALUATE ANY-CHILD-TKN

      *        -----
      *        For GETNAME, print the welcome message
      *        -----
               WHEN GET-NAME-TKN

      *          Save the channel name for future use
                 MOVE ANY-CHILD-CHAN TO GET-NAME-CHAN

                 EXEC CICS GET CONTAINER ( GETNAME-CONTAINER )
                                 CHANNEL ( GET-NAME-CHAN )
                                 INTO    ( CUSTOMER-NAME )
                                 RESP    ( COMMAND-RESP )
                                 RESP2   ( COMMAND-RESP2 )
                 END-EXEC

                 PERFORM CHECK-COMMAND

                 INITIALIZE STATUS-MSG
                 STRING 'Welcome '
                        DELIMITED BY SIZE
                        CUSTOMER-NAME
                        DELIMITED BY SIZE
                      INTO MSG-TEXT
                 PERFORM PRINT-STATUS-MESSAGE

      *        -----
      *        For ACCTCURR, print the account details
      *        -----
               WHEN ACCTCURR-TKN

      *          Save the channel name for future use
                 MOVE ANY-CHILD-CHAN TO ACCTCURR-CHAN

                 EXEC CICS GET CONTAINER ( ACCTCURR-CONTAINER )
                                 CHANNEL ( ACCTCURR-CHAN )
                                 INTO    ( CURRENT-ACCOUNTS )
                                 RESP    ( COMMAND-RESP )
                                 RESP2   ( COMMAND-RESP2 )
                 END-EXEC

                 PERFORM CHECK-COMMAND
                 PERFORM PRINT-CURRENT-ACCOUNTS-DETAILS

      *        -----
      *        For ACCTPTNR, print the partner account details
      *        -----
               WHEN ACCTPTNR-TKN

      *          Save the channel name for future use
                 MOVE ANY-CHILD-CHAN TO ACCTPTNR-CHAN

                 EXEC CICS GET CONTAINER ( ACCTPTNR-CONTAINER )
                               CHANNEL   ( ACCTPTNR-CHAN )
                               INTO      ( PARTNER-ACCOUNTS )
                               RESP      ( COMMAND-RESP )
                               RESP2     ( COMMAND-RESP2 )
                 END-EXEC

                 PERFORM CHECK-COMMAND
                 PERFORM PRINT-PARTNER-ACCOUNTS-DETAILS

      *        -----
      *        Error: Unknown child is returned
      *        -----
               WHEN OTHER
                 INITIALIZE STATUS-MSG
                 STRING '*** Unknown child token: '
                        DELIMITED BY SIZE
                        ANY-CHILD-TKN
                        DELIMITED BY SIZE
                      INTO MSG-TEXT
                 PERFORM PRINT-STATUS-MESSAGE

                 PERFORM WEBHOME-ERROR

             END-EVALUATE

      * End of FETCH ANY loop
           END-PERFORM

      * -----
      * Provide new business directive of Loan up-sell.
      * Asynchronously call personalised loan rate generator.
      * -----

      *    -----
      *    Pass the details of all of the customer's accounts
      *    to provide a personalised loan quote
      *    -----
           EXEC CICS PUT CONTAINER ( ACCOUNTS-CONTAINER )
                           FROM    ( CUSTOMER-ACCOUNTS )
                           CHANNEL ( MYCHANNEL)
                           RESP    ( COMMAND-RESP )
                           RESP2   ( COMMAND-RESP2 )
           END-EXEC

           PERFORM CHECK-COMMAND

      * --------------------------------------------------------
      * Asynchronously run GETL to get customers
      * personalised loan rate
      * --------------------------------------------------------
           EXEC CICS RUN TRANSID ( GETLOAN-TRAN )
                         CHANNEL ( MYCHANNEL )
                         CHILD   ( GET-LOAN-TKN )
                         RESP    ( COMMAND-RESP )
                         RESP2   ( COMMAND-RESP2 )
           END-EXEC

           PERFORM CHECK-COMMAND

      *    -----
      *    Before fetching (and blocking) on the loan quote results
      *    Check to see if we should apply a TIMEOUT.
      *    Typically from a FILE or DB2 look up -
      *    for simplicity we will use a TSQ.
      *    -----
           MOVE 8 TO TIMEOUT-LEN
           EXEC CICS READQ TS QUEUE  ( TSQ-NAME )
                              ITEM   ( 1 )
                              INTO   ( TSQ-TIMEOUT )
                              LENGTH ( TIMEOUT-LEN )
                              RESP   ( COMMAND-RESP )
                              RESP2  ( COMMAND-RESP2 )
           END-EXEC

           IF COMMAND-RESP = DFHRESP(NORMAL)
           THEN

      *      -----
      *      Found a timeout value to use on the FETCH of the quote
      *      -----
             MOVE TSQ-TIMEOUT(1:TIMEOUT-LEN) TO LOAN-RATE-TIMEOUT

             INITIALIZE STATUS-MSG
             STRING 'Timeout of '
                      DELIMITED BY SIZE
                      TSQ-TIMEOUT
                      DELIMITED BY SPACE
                      ' milliseconds to get loan rate quote.'
                      DELIMITED BY SIZE
                    INTO MSG-TEXT
             PERFORM PRINT-STATUS-MESSAGE 

           ELSE

      *      -----
      *      Did not find a timeout value. Continue with NO timeout
      *      A TIMEOUT(0) parameter on the FETCH indicates no timeout
      *      -----

             MOVE 0 TO LOAN-RATE-TIMEOUT

             INITIALIZE STATUS-MSG
             MOVE 'Timeout not set for loan rate quote.' TO MSG-TEXT
             PERFORM PRINT-STATUS-MESSAGE
           END-IF

      * --------------------------------------------------------------
      * Perform the FETCH of loan rate
      * --------------------------------------------------------------
           EXEC CICS FETCH CHILD      ( GET-LOAN-TKN )
                           TIMEOUT    ( LOAN-RATE-TIMEOUT )
                           CHANNEL    ( GET-LOAN-CHAN )
                           COMPSTATUS ( CHILD-RETURN-STATUS )
                           ABCODE     ( CHILD-RETURN-ABCODE )
                           RESP       ( COMMAND-RESP )
                           RESP2      ( COMMAND-RESP2 )
           END-EXEC

      *    -----
      *    Check if the FETCH of the child's results timed out
      *    -----
           IF COMMAND-RESP = DFHRESP(NOTFINISHED) AND COMMAND-RESP2 = 53
           THEN
             INITIALIZE STATUS-MSG
             MOVE
              'Abandoned loan quote because it took too long!'
              TO MSG-TEXT
             PERFORM PRINT-STATUS-MESSAGE

           ELSE

             PERFORM CHECK-COMMAND
             PERFORM CHECK-CHILD

      *      -----
      *      Successful response from the child.
      *      Get the personalised loan quote
      *      -----
             EXEC CICS GET CONTAINER ( GETLOAN-CONTAINER )
                           CHANNEL   ( GET-LOAN-CHAN )
                           INTO      ( CUSTOMER-LOAN-RATE )
                           RESP      ( COMMAND-RESP )
                           RESP2     ( COMMAND-RESP2 )
             END-EXEC

             PERFORM CHECK-COMMAND

      *      -----
      *      Finally, display the loan quote
      *      -----

             INITIALIZE STATUS-MSG
             STRING 'Personalised Loan Rate: '
                    DELIMITED BY SIZE
                    CUSTOMER-LOAN-RATE
                    DELIMITED BY SPACE
                    ' %'
                    DELIMITED BY SIZE
                  INTO MSG-TEXT
             PERFORM PRINT-STATUS-MESSAGE

           END-IF

      * Send a message to the screen to
      * notify terminal user of completion
           MOVE 'COMPLETE' TO CURRENT-STATUS
           PERFORM PRINT-TEXT-TO-SCREEN

      * Display a conclusion message that also includes a timestamp
           INITIALIZE STATUS-MSG
           MOVE 'Ended Web banking log-on data retrieval' TO MSG-TEXT
           PERFORM PRINT-STATUS-MESSAGE

      * Return at end of program
           EXEC CICS RETURN
           END-EXEC
           .
      * --------------------------------------------------------------
      * End of the main code execution
      * --------------------------------------------------------------

      * --------------------------------------------------------------
      * Below are helpful procedures and routines
      * --------------------------------------------------------------

      * Retrieve the customer account number, which should be
      * specified on the terminal command after the transaction ID.
       GET-INPUT-ACCOUNT-NUMBER.
           EXEC CICS RECEIVE INTO       ( READ-INPUT )
                             LENGTH     ( READ-INPUT-LENGTH )
                             NOTRUNCATE
                             RESP       ( COMMAND-RESP )
                             RESP2      ( COMMAND-RESP2 )
           END-EXEC

           IF INPUTACCNUM = '    '
           THEN 
      * if we failed to locate an account number, continue with 9999
             MOVE '9999' TO CUST-NO-IN
             MOVE '9999' TO ACCOUNT-NUM
           ELSE
             MOVE INPUTACCNUM TO CUST-NO-IN
             MOVE INPUTACCNUM TO ACCOUNT-NUM
           END-IF

      * Send a message to the screen to
      * notify terminal user that the application is running
           PERFORM PRINT-TEXT-TO-SCREEN
           .

      * Print current account details
       PRINT-CURRENT-ACCOUNTS-DETAILS.
           IF NUMBER-OF-ACCOUNTS OF CURRENT-ACCOUNTS > 0 THEN
             MOVE 1 TO COUNTER
             PERFORM UNTIL COUNTER > 
                       NUMBER-OF-ACCOUNTS OF CURRENT-ACCOUNTS
               INITIALIZE STATUS-MSG
               STRING 'Acc: '
                      DELIMITED BY SIZE
                      ACCT-NUMBER OF CURRENT-ACCOUNTS (COUNTER)
                      DELIMITED BY SPACE
                      ' Bal: $'
                      DELIMITED BY SIZE
                      BALANCE OF CURRENT-ACCOUNTS (COUNTER)
                      DELIMITED BY SIZE
                      ' Overdraft: $'
                      DELIMITED BY SIZE
                      OVERDRAFT OF CURRENT-ACCOUNTS (COUNTER)
                      DELIMITED BY SIZE
                    INTO MSG-TEXT
               PERFORM PRINT-STATUS-MESSAGE
               ADD 1 TO COUNTER
             END-PERFORM
           END-IF
           .

      * Print partner account details
       PRINT-PARTNER-ACCOUNTS-DETAILS.
           IF NUMBER-OF-ACCOUNTS OF PARTNER-ACCOUNTS > 0 THEN
             MOVE 1 TO COUNTER
             PERFORM UNTIL COUNTER >
                       NUMBER-OF-ACCOUNTS OF PARTNER-ACCOUNTS
               INITIALIZE STATUS-MSG
               STRING 'Acc: '
                      DELIMITED BY SIZE
                      ACCT-NUMBER OF PARTNER-ACCOUNTS (COUNTER)
                      DELIMITED BY SPACE
                      ' Bal: $'
                      DELIMITED BY SIZE
                      BALANCE OF PARTNER-ACCOUNTS (COUNTER)
                      DELIMITED BY SIZE
                      ' Overdraft: $'
                      DELIMITED BY SIZE
                      OVERDRAFT OF PARTNER-ACCOUNTS (COUNTER)
                      DELIMITED BY SIZE
                    INTO MSG-TEXT
               PERFORM PRINT-STATUS-MESSAGE
               ADD 1 TO COUNTER
             END-PERFORM
           END-IF
           .

      * Print status message
       PRINT-STATUS-MESSAGE.
           MOVE FUNCTION CURRENT-DATE(13:2) TO MSG-SEC
           MOVE FUNCTION CURRENT-DATE(11:2) TO MSG-MIN
           MOVE FUNCTION CURRENT-DATE(9:2)  TO MSG-HOUR

           DISPLAY STATUS-MSG
           .

      * update terminal screen with progress status
       PRINT-TEXT-TO-SCREEN.
           EXEC CICS SEND TEXT FROM ( TERMINAL-STATUS )
                     TERMINAL WAIT
                     FREEKB
                     ERASE
           END-EXEC
           .

      * Routine to check command
       CHECK-COMMAND.
           IF COMMAND-RESP NOT = DFHRESP(NORMAL)
           THEN
             PERFORM WEBHOME-ERROR
           END-IF
           .

      * Routine to check child completion status
      * For simplicity, we simply exit.
      * It could be useful to print further details, such as abcode
       CHECK-CHILD.
           IF CHILD-RETURN-STATUS NOT = DFHVALUE(NORMAL)
           THEN
             PERFORM WEBHOME-ERROR
           END-IF
           .

      * Error path processing to write messages and abend
       WEBHOME-ERROR.
      * Send a error status message
           INITIALIZE STATUS-MSG
           MOVE '*** Error occurred in WEBHOME.' TO MSG-TEXT
           PERFORM PRINT-STATUS-MESSAGE

      * Send a message to the terminal screen 
           MOVE 'FAILED' TO CURRENT-STATUS
           PERFORM PRINT-TEXT-TO-SCREEN

           EXEC CICS ABEND ABCODE('WEBH') NODUMP END-EXEC
           .

       END PROGRAM 'WEBHOME'.