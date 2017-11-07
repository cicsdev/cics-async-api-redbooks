<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<%@page import="banking.CustomerAccounts"%>
<%@page import="java.util.Map"%>
<%
/*
 * Licensed Materials - Property of IBM
 *
 * SAMPLE
 *
 * (c) Copyright IBM Corp. 2017 All Rights Reserved
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp
 */
  String customer = request.getParameter("customer");

  if (customer == null)
  {
    customer = "9999";
  }

  CustomerAccounts accountsPage = new CustomerAccounts(customer);
  Map<String, String> content = accountsPage.getContent();
%>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<link rel="stylesheet" type="text/css" href="theme.css">
<title>my bank</title>
</head>
<body>
	<h1>my bank</h1>

	<div id="main">

		<p>
			Hello, <strong><%=content.get("name")%></strong>.
		</p>


		<h2>my accounts</h2>

		<p>
			Balances at
			<%=content.get("timedate")%>.
		</p>

		<h3>current accounts</h3>

		<table>
			<tr>
				<td><strong>Account number</strong></td>
				<td><strong>Balance</strong></td>
				<td><strong>Overdraft</strong></td>
			</tr>
			<%=content.get("current-accounts")%>
		</table>

		<h3>other accounts</h3>

		<table>
			<tr>
				<td><strong>Account number</strong></td>
				<td><strong>Balance</strong></td>
				<td><strong>Overdraft</strong></td>
			</tr>
			<%=content.get("partner-accounts")%>
		</table>

		<p id="smallprint">
			For more details see <a href="http://ibm.biz/async-redbooks">ibm.biz/async-redbooks</a>,
			and find the code on <a
				href="https://github.com/cicsdev/cics-async-api-redbooks">GitHub</a>.
		</p>
	</div>

	<div id="side">
		<h2>need a loan?</h2>
		<p><%=content.get("loan-rate")%></p>
		<p>
			<a
				href="https://developer.ibm.com/cics/2016/07/22/introducing-asynchronous-api/">Click
				here</a> to learn more.
		</p>
		<p id="smallprint">
			Loan fetched using a timeout value of
			<%=content.get("timeout")%>ms.
		</p>
	</div>
</body>
</html>
