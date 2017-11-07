package banking;

import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;

public class BankAccounts
{
  /**
   * The number of accounts the customer has.
   */
  public short numberOfAccounts;

  /**
   * Each current account holds: account number, balance, overdraft limit.
   */
  private List<List<String>> accounts = new ArrayList<List<String>>();


  public BankAccounts(byte[] input)
  {
    /*
     * Determine how many accounts have been returned: the COBOL program returns
     * a two byte integer.
     */


    ByteBuffer bb = ByteBuffer.allocate(2);
    bb.put(input, 0, 2);
    numberOfAccounts = bb.getShort(0);


    /*
     * Create a new string (assuming a string encoding) from the remaining
     * bytes,
     * skipping the first two bytes already read.
     */


    String details = null;
    try
    {
      details =
          new String(input, 2, 3 * 8 * numberOfAccounts,
              CustomerAccounts.CODEPAGE);
    }
    catch (UnsupportedEncodingException e)
    {
      e.printStackTrace();
    }


    /*
     * Parse the account details returned for each account. For each account,
     * obtain the substring for that account detail item (account number,
     * balance, or overdraft), and add it to the list.
     */


    for (short i = 0; i < numberOfAccounts; i++)
    {
      List<String> account = new ArrayList<String>();
      for (int j = 0; j < 3; j++)
      {
        account.add(details.substring((i * 24) + (j * 8),
            (i * 24) + ((j + 1) * 8)).trim());
      }
      accounts.add(account);
    }
  }


  /**
   * Converts the {@link #accounts} generated in the constructor into a String
   * holding the HTML for display as a set of table rows.
   *
   * @return HTML string of {@code <tr>} and {@code <td>} tags to construct a
   *         table of accounts.
   */
  public String htmlRows()
  {
    String currentAccountTable = "";

    // For each account, create the HTML output
    for (List<String> account : accounts)
    {
      currentAccountTable += "<tr>";
      for (int i = 0; i < 3; i++)
      {
        currentAccountTable += "<td>";
        if (i > 0)
        {
          currentAccountTable += "$";
        }
        currentAccountTable += account.get(i) + "</td>";
      }
      currentAccountTable += "</tr>";
    }

    return currentAccountTable;
  }


  public List<List<String>> getAccounts()
  {
    return accounts;
  }
}
