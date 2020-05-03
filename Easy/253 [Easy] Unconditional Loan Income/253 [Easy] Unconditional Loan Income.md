---
draft: true
----

# [253 (Easy) Unconditional Loan Income](https://www.reddit.com/r/dailyprogrammer/comments/44qzj5/20160208_challenge_253_easy_unconditional_loan/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

Unconditional Loan Income is a private or public (social) program that uses "soft loans" whose only repayment obligation is a royalty on future income.

(http://www.naturalfinance.net/2016/02/unconditional-loan-income-ubi-pilot.html)
Special considerations for core/simple test are:

When repayments are made, the first loan in queue (first loan taken out) is repaid with the payment.  Special considerations for bonus are:

# sample test
Given an interest rate, annual loan amount, starting age, royalty rate under age 65, clawback balance trigger, royalty rate over 65 and an annual (assumed) income stream, calculate total repayments and profit or loss:

# sample input
interest rate: 2%
annual loan amount: $15000
start age: 18
clawback balance trigger: $100000
royalty rate (under 65): 20%
royalty rate (over 65): 40%
income stream: (in thousands)


```
0 0 20 20 20 20 20 20 20 20 20 20 30 30 30 30 30 30 30 30 30 30 40 40 40 40 40 40 40 40 40 40 50 50 50 50 50 50 50 50 50 50 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
```
# sample output (in thousands)
Overall loans taken: $1080
Repayments from income: $280
Repayments from benefit clawbacks: $270
Ending balance with interest:  $1169.09

# input #2
interest rate: 2%
annual loan amount: $15000
start age: 18
clawback balance trigger: $100000
royalty rate (under 65): 20%
royalty rate (over 65): 40%
income stream: (in thousands)


```
0 0 30 30 30 30 30 30 30 30 30 30 40 40 40 40 40 40 40 40 40 40 50 50 50 50 50 50 50 50 50 50 60 60 60 60 60 60 60 60 60 60 100 120 140 160 200 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10
```
# output #2 (in thousands)
Overall loans taken: $1005
Repayments from income: $584
Repayments from benefit clawbacks: $237
Ending balance with interest:  $509.487  

# bonus
Previous format allows calculations with a single running total.  Adding the bonus special considerations means tracking each $15000 loan individually.


----
## **DISCLAIMER**
This prompt has been adapted from [253 [Easy] Unconditional Loan Income](https://www.reddit.com/r/dailyprogrammer/comments/44qzj5/20160208_challenge_253_easy_unconditional_loan/
)
