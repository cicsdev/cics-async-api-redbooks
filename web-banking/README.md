# CICS Asynchronous API Web Banking Homepage Example

This is an example application used by the IBM Redbooks publication,
[_IBM CICS Asynchronous API: Concurrent Processing Made
Simple_][book]. This application acts as a data gathering application
to populate the information for a Web banking homepage for a fictional
bank.

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


### COBOL Programs and their Resource Definitions
The source code in folder web-banking-homepage tracks the changes made
as the Redbooks publication progresses.  Use the tags to identify the
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
Redbooks publication will prompt to add transactions `PTNR`, `GETN`,
`ACUR`, and `GETL`.

A CICS Bundle project, `AsyncRedbooksWebBankingBundle`, is provided in
the [`etc`](etc/) directory, along side the bundles for the Java
example in Chapter 6.

To use `AsyncRedbooksWebBankingBundle`:

1. Import it into CICS Explorer
2. Edit the LIBRARY resource contained within it to point to the
   dataset name where your compiled COBOL programs are
3. Export the bundle to zFS and define and install it in your CICS
   region.


### Java Web Frontend Application
Two Eclipse projects are provided in the [`etc`](etc/) directory:

- [`AccountServices`](etc/AccountServices/)
- [`AccountServicesBundle`](etc/AccountServicesBundle/)

The first is the dynamic web project that is explained in Chapter 6 of
the Redbooks publication. The second project is a CICS Bundle to be
able to deploy the dynamic web project to Liberty in CICS. The assumed
name of the `JVMSERVER` resource is `DFHWLP`. This should be changed
to match the name of your Liberty `JVMSERVER` resource, by editing
[`AccountServices.warbundle`](etc/AccountServicesBundle/AccountServices.warbundle)
in `AccountServicesBundle`.

After successfully deploying this CICS Bundle to your CICS region,
Liberty's `messages.log` will print the URL of the web
application. Use a web browser to visit the page and drive the
business logic.


## Running the Example


### Using a CICS Terminal
At the CICS terminal screen, enter the transaction WEBH, followed by a
four-digit customer number, such as 0001.

For example:

```
WEBH 0001
```


## Using z/OS Provisioning Toolkit
If you have [z/OS Provisioning Toolkit][zospt] set up at your
workplace, you can use the [`zosptfile`](etc/zosptfile) we've included
to automate the provisioning of a CICS region with the resource
definitions and Java components installed. To use this:

1. Download the [zip file of this repository][zip]
2. Copy it across to the zFS partition on your LPAR (ensuring it's
   copied in binary format)
3. From a shell when connected to USS on the LPAR, extract the zip
   file, e.g.,

   ```bash
   jar xf cics-async-api-redbooks-master.zip
   ```

4. Edit the library resource in the CICS Bundle,
   [`AsyncRedbooksWebBankingBundle/ASYNCLIB.library`][asynclib], to
   change its `dsname01` property to point to your dataset with the
   compiled COBOL modules
5. Edit the
   [`AccountServicesBundle/AccountServices.warbundle`][warbundle]'s
   jvmserver property, if you've edited the default of `DFHWLP` that
   z/OS PT provides in its template
4. Build the image:

   ```bash
   zospt build -t cics_async_redbooks cics-async-api-redbooks-master/web-banking/etc/
   ```

5. Run the image:

   ```bash
   zospt run cics_async_redbooks
   ```

6. After the CICS region starts successfully, use a web browser to
   connect to

   ```
   http://[hostname]:[port]/AccountServices
   ```

   specific to your LPAR's hostname, and the port number assigned to
   your CICS region by z/OS PT.


## License
This project is licensed under [Apache License Version 2.0](../LICENSE).




[zospt]: https://developer.ibm.com/mainframe/products/zospt/
[zip]: https://github.com/cicsdev/cics-async-api-redbooks/archive/master.zip
[asynclib]: etc/AsyncRedbooksWebBankingBundle/ASYNCLIB.library
[warbundle]: etc/AccountServicesBundle/AccountServices.warbundle
[book]: https://www.redbooks.ibm.com/Redbooks.nsf/RedbookAbstracts/sg248411.html?Open
