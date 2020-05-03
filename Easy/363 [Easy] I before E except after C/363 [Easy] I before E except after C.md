---
draft: true
----

# [363 (Easy) I before E except after C](https://old.reddit.com/r/dailyprogrammer/comments/8q96da/20180611_challenge_363_easy_i_before_e_except/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Background
"I before E except after C" is perhaps the most famous English spelling rule. For the purpose of this challenge, the rule says:

A word also follows the rule if neither "ei" nor "ie" appears anywhere in the word. Examples of words that follow this rule are:


```
fiery hierarchy hieroglyphic
ceiling inconceivable receipt
daily programmer one two three
```
There are many exceptions that don't follow this rule, such as:


```
sleigh stein fahrenheit
deifies either nuclei reimburse
ancient juicier societies
```
# Challenge
Write a function that tells you whether or not a given word follows the "I before E except after C" rule.


```
check("a") => true
check("zombie") => true
check("transceiver") => true
check("veil") => false
check("icier") => false
```
# Optional Bonus 1
How many words in the enable1 word list are exceptions to the rule? (The answer is 4 digits long and the digits add up to 18.)

(https://norvig.com/ngrams/enable1.txt)
# Optional Bonus 2
This one is subjective and there's no best answer. Come up with your own "I before E" rule. Your rule must:

For instance, I just came up with a rule "I before E, except when followed by G". This rule has 1,544 exceptions in the enable1 word list. How many exceptions does your rule have?


----
## **DISCLAIMER**
This prompt has been adapted from [363 [Easy] I before E except after C](https://old.reddit.com/r/dailyprogrammer/comments/8q96da/20180611_challenge_363_easy_i_before_e_except/
)
