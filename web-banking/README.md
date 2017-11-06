# CICS Asynchronous API Web Banking Homepage Example

This is an example application used by the CICS Asynchronous API
Redbooks publication. This application acts as a data gathering
application to populate the information for a Web banking homepage for
a fictional bank.

The main application program `WEBHOME` initially requests information
from `GETNAME` and `ACCTCURR`, to retrieve details on a customer and
their accounts held.  The Redbooks publication steps through the
process of adding an additional service `ACCTPTNR`, to provide details
on accounts held by a banking partner. The addition of this service
call is made asynchronously.  The application is optimised to call all
services asynchronously and to consume results from child tasks as
soon as they are available. This greatly reduces the response time of
the application.  A further business opportunity is added to the
application to provide a personalised loan quote, `GETLOAN`. The
`WEBHOME` application retrieves a loan quote, whilst using a timeout
to ensure an agreed response time is met.  Lastly, the `WEBHOME`
program is transformed from a COBOL application into a Java
application, whilst still interacting with service providers in other
languages.

This fictional example is based on real life situations, showcasing
how the CICS asynchronous API in CICS TS V5.4 can provide robust,
responsive applications.

## Set Up
The source code in folder web-banking-homepage tracks the changes made
as the Rebooks publication progresses.  Use the tags to identify the
level of the application at a particular point in the examples
history.

1. Download the source code from the GitHub repository at the required
   tag version, to your preferred location.
2. Compile the COBOL source code and make the results available in a
   location accessible by CICS TS V5.4 and above. This can be in a RPL
   load library, or a load library dataset defined in a LIBRARY
   resource in CICS.
3. Define the transactions and programs in your CSD.

Here are sample CSD definitions:

```
DEFINE PROGRAM(WEBHOME) GROUP(ASYNCAPI)
DEFINE TRANSACTION(WEBH) GROUP(ASYNCAPI) PROGRAM(WEBHOME)
       DESCRIPTION(Web Banking Homepage for the CICS Asynchronous API example)

DEFINE PROGRAM(ACCTCURR) GROUP(ASYNCAPI)
DEFINE TRANSACTION(ACUR) GROUP(ASYNCAPI) PROGRAM(ACCTCURR)
       DESCRIPTION(Bank account details for the CICS Asynchronous API example) 

DEFINE PROGRAM(GETNAME) GROUP(ASYNCAPI)
DEFINE TRANSACTION(GETN) GROUP(ASYNCAPI) PROGRAM(GETNAME)
       DESCRIPTION(Customer name for the CICS Asynchronous API example)

DEFINE PROGRAM(ACCTPTNR) GROUP(ASYNCAPI)
DEFINE TRANSACTION(PTNR) GROUP(ASYNCAPI) PROGRAM(ACCTPTNR)
       DESCRIPTION(Partner bank account details for the CICS Asynchronous API example) 

DEFINE PROGRAM(GETLOAN) GROUP(ASYNCAPI)
DEFINE TRANSACTION(GETL) GROUP(ASYNCAPI) PROGRAM(GETLOAN)
       DESCRIPTION(Personalized loan rate for the CICS Asynchronous API example)
```
 
Note that the step-by-step instructions in the CICS Asynchronous API
Rebooks publication will prompt to add transactions `PTNR`, `GETN`,
`ACUR`, and `GETL`.

## Running the Example

### Using a CICS Terminal
At the CICS terminal screen, enter the transaction WEBH, followed by a
four-digit customer number, such as 0001.

For example:

```
WEBH 0001
```

## License
This project is licensed under [Apache License Version 2.0](../LICENSE).
