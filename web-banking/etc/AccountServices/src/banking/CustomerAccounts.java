package banking;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import com.ibm.cics.server.AsyncService;
import com.ibm.cics.server.AsyncServiceImpl;
import com.ibm.cics.server.CCSIDErrorException;
import com.ibm.cics.server.Channel;
import com.ibm.cics.server.ChannelErrorException;
import com.ibm.cics.server.ChildResponse;
import com.ibm.cics.server.ChildResponse.CompletionStatus;
import com.ibm.cics.server.CodePageErrorException;
import com.ibm.cics.server.ContainerErrorException;
import com.ibm.cics.server.InvalidRequestException;
import com.ibm.cics.server.InvalidTransactionIdException;
import com.ibm.cics.server.NotAuthorisedException;
import com.ibm.cics.server.NotFoundException;
import com.ibm.cics.server.ResourceDisabledException;
import com.ibm.cics.server.Task;

public class CustomerAccounts
{
  private byte[] accountNumber;
  public static String CODEPAGE = "CP037";


  public CustomerAccounts(String accountNumber)
  {
    try
    {
      // Ensure that the account number is 4 digits in length
      this.accountNumber =
          String.format("%04d", Integer.parseInt(accountNumber)).getBytes(
              CODEPAGE);
    }
    catch (UnsupportedEncodingException e)
    {
      e.printStackTrace();
    }
  }


  /**
   * Start child tasks to get content asynchronously. Return the content with
   * keys:
   *
   * - "name"
   * - "current-accounts"
   * - "partner-accounts"
   * - "loan-rate"
   *
   * @return map of content strings.
   */
  public Map<String, String> getContent()
  {
    final String accountPartnerTran = "PTNR";
    final String getCustomerNameTran = "GETN";
    final String getCurrentAccountTran = "ACUR";
    final String getLoanTran = "GETL";

    final String inputContainer = "INPUTCONTAINER";

    Channel myChannel = null;
    Set<Future<ChildResponse>> childTasks =
        new HashSet<Future<ChildResponse>>();
    AsyncService async = new AsyncServiceImpl();

    Map<String, String> returnContent = new HashMap<String, String>();


    /*
     * Get the timeout value from a properties file, or use the default of "0".
     * Used later with get() method.
     */


    long timeout = 0;
    Properties prop = new Properties();
    prop.setProperty("timeout", "0");
    try
    {
      FileInputStream input = new FileInputStream("async.properties");
      prop.load(input);
      if (prop.getProperty("timeout") != null)
      {
        timeout = Long.parseLong(prop.getProperty("timeout"));
      }
    }
    catch (IOException e)
    {
      System.out.println("Failed to find or read file async.properties");
    }

    returnContent.put("timeout", Long.toString(timeout));


    System.out.println("Started Web banking log-on data retrieval");

    // Store the time at which we make the requests for bank balances
    String time =
        LocalDateTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss"));
    String date =
        LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd"));
    returnContent.put("timedate", time + " on " + date);


    /*
     * Populate a channel to pass to the child tasks with the input account
     * number.
     */


    Task t = Task.getTask();
    try
    {
      myChannel = t.createChannel("MYCHANNEL");
      myChannel.createContainer(inputContainer).put(accountNumber);
    }
    catch (ChannelErrorException | ContainerErrorException
        | InvalidRequestException | CCSIDErrorException
        | CodePageErrorException e)
    {
      e.printStackTrace();
    }


    /*
     * Start the three child tasks.
     */


    // Get account details from the partner bank
    Future<ChildResponse> accountPartner = null;
    try
    {
      accountPartner = async.runTransactionId(accountPartnerTran, myChannel);
    }
    catch (InvalidRequestException | InvalidTransactionIdException
        | ChannelErrorException | NotAuthorisedException
        | ResourceDisabledException e)
    {
      e.printStackTrace();
    }
    childTasks.add(accountPartner);

    // Retrieve the customer's name
    Future<ChildResponse> customerName = null;
    try
    {
      customerName = async.runTransactionId(getCustomerNameTran, myChannel);
    }
    catch (InvalidRequestException | InvalidTransactionIdException
        | ChannelErrorException | NotAuthorisedException
        | ResourceDisabledException e)
    {
      e.printStackTrace();
    }
    childTasks.add(customerName);

    // Get customer's account details
    Future<ChildResponse> currentAccount = null;
    try
    {
      currentAccount = async.runTransactionId(getCurrentAccountTran, myChannel);
    }
    catch (InvalidRequestException | InvalidTransactionIdException
        | ChannelErrorException | NotAuthorisedException
        | ResourceDisabledException e)
    {
      e.printStackTrace();
    }
    childTasks.add(currentAccount);


    /*
     * Collect all the results from the 3 started children. We don't know the
     * order of the returning children, so we'll compare each time.
     */


    ChildResponse anyResponse = null;
    Iterator<Future<ChildResponse>> childIterator = childTasks.iterator();

    while (childIterator.hasNext())
    {
      try
      {
        anyResponse = async.getAny();

        if (anyResponse.equals(customerName))
        {
          returnContent.put("name",
              decodeContainerBytes(anyResponse, "GETNAMECONTAINER"));
        }
        else if (anyResponse.equals(currentAccount))
        {
          // Convert the bytes from the returned container with current account
          // details to a list of accounts and their details
          BankAccounts currentAccounts =
              new BankAccounts(anyResponse.getChannel()
                  .getContainer("ACCTCURRCONT").getNoConvert());

          returnContent.put("current-accounts", currentAccounts.htmlRows());
        }
        else if (anyResponse.equals(accountPartner))
        {
          // Convert the bytes from the returned container with partner account
          // details to a list of accounts and their details
          BankAccounts partnerAccounts =
              new BankAccounts(anyResponse.getChannel()
                  .getContainer("ACCTPTNRCONT").getNoConvert());

          returnContent.put("partner-accounts", partnerAccounts.htmlRows());

        }
        else
        {
          System.out.println("*** Unknown child fetched ***");
        }
      }
      catch (InvalidRequestException | NotFoundException
          | ContainerErrorException | ChannelErrorException
          | CCSIDErrorException | CodePageErrorException e)
      {
        e.printStackTrace();
      }

      childIterator.next();
    }


    /*
     * Provide new business directive of loan up-sell: asynchronously call
     * personalized loan rate generator
     */


    // Get customer's personalized loan rate
    Future<ChildResponse> loanRate = null;
    try
    {
      loanRate = async.runTransactionId(getLoanTran, myChannel);
    }
    catch (InvalidRequestException | InvalidTransactionIdException
        | ChannelErrorException | NotAuthorisedException
        | ResourceDisabledException e)
    {
      e.printStackTrace();
    }

    // Perform the FETCH of loan rate with the specified timeout
    ChildResponse returnedLoanRate = null;

    System.out.println("Using timeout value of " + timeout
        + " milliseconds to get loan rate quote");

    try
    {
      returnedLoanRate = loanRate.get(timeout, TimeUnit.MILLISECONDS);
    }
    catch (InterruptedException | ExecutionException e)
    {
      e.printStackTrace();
    }
    catch (TimeoutException e)
    {
      returnContent.put("loan-rate",
          "You could be eligible for great loan rates.");
      return returnContent;
    }

    // Print the personalized loan amount, if it's returned normally
    if (returnedLoanRate.getCompletionStatus().equals(CompletionStatus.NORMAL))
    {
      returnContent.put("loan-rate",
          "Great news! We've managed to obtain a great interest rate of "
              + decodeContainerBytes(returnedLoanRate, "GETLOANCONTAINER")
              + "% for you.");
    }

    System.out.println("Ended Web banking log-on data retrieval");

    return returnContent;
  }


  /**
   * Return the contents of the named container from the {@code response} object
   * as a {@code String}.
   *
   * @param response child response to get the channel from
   * @param containerName name of the container to find
   * @return decoded string container contents
   */
  private String decodeContainerBytes(ChildResponse response,
      String containerName)
  {
    byte[] returnValue = null;

    try
    {
      returnValue = response.getChannel().getContainer(containerName).get();
    }
    catch (ContainerErrorException | ChannelErrorException
        | CCSIDErrorException | CodePageErrorException e)
    {
      System.out.println("Container error: " + containerName);
    }

    try
    {
      return new String(returnValue, CODEPAGE).trim();
    }
    catch (UnsupportedEncodingException e)
    {
      e.printStackTrace();
      return null;
    }
  }
}
