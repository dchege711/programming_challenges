# [255 (Intermediate) Ambiguous Bases](https://www.reddit.com/r/dailyprogrammer/comments/47docs/20160224_challenge_255_intermediate_ambiguous/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description:
Due to an unfortunate compression error your lucky number in base n was compressed to a simple string where the conversion to decimal has potentially many values.

Normal base n numbers are strings of characters, where each character represents a value from 0 to (n-1) inclusive. The numbers we are dealing with here can only use digits though, so some "digits" span multiple characters, causing ambiguity.

For example "A1" in normal hexadecimal would in our case be "101" as "A" converts to 10, as "A" is the 10th character in base 16 

"101" is can have multiple results when you convert from ambiguous base 16 to decimal as it could take on the possible values:


```
1*16^2 + 0*16^1 + 1*16^0  (dividing the digits as [1][0][1])
 10*16^1 + 1*16^0 (dividing the digits as [10][1])
```
A few notes:


```
[1][01]
```

```
01
```
# Input:
You will be given a string of decimal values ("0123456789") and a base n.

# Output:
Convert the input string to all possible unique base 10 values it could take on, sorted from smallest to largest.

# Challenge Inputs

```
101 2
```

```
101 16
```

```
120973 25
```
# Bonus Inputs

```
25190239128039083901283 100
```

```
251902391280395901283 2398
```
The first 10,000 values of each Bonus output are pasted here respectively:

http://pastebin.com/QjP3gazp

(http://pastebin.com/QjP3gazp)
http://pastebin.com/ajr9bN8q

(http://pastebin.com/ajr9bN8q)
# Finally
Credit for this challenge goes to by /u/wwillsey, who proposed it in /r/dailyprogrammer_ideas. Have your own neat challenge idea? Drop by and show it off!

(/u/wwillsey)
(/r/dailyprogrammer_ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [255 [Intermediate] Ambiguous Bases](https://www.reddit.com/r/dailyprogrammer/comments/47docs/20160224_challenge_255_intermediate_ambiguous/
)
