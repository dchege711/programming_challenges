---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [351 (Easy) Cricket Scoring](https://www.reddit.com/r/dailyprogrammer/comments/7x81yg/20180213_challenge_351_easy_cricket_scoring/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
Cricket is a bat-and-ball game played between two teams of 11 players each on a field at the centre of which is a rectangular pitch. The game is played by 120 million players in many countries, making it the world's second most popular sport!  

There are 2 batsmen standing on two bases and the ball is played to the strike base. Batsmen run between their bases to increases their 'runs'. If a batsman gets out, a new batsman arrives to their base.
This is only a simplified version of the rules  

Let's look at a typical score: 1.2wW6.2b34
There are 5 types of characters:  

'w' - A wide, 1 run to the team but not to any particular batsman.
The difference between 'w' and 'b' is that a 'b' counts as a ball but 'w' is not a legal ball.  

'W' - Strike batsman is out. A new player takes his place, if there is any player left to play out of 11 available. If not, the innings is complete.  

Additional Rules:  

# Formal Inputs & Outputs
## Input description
Ball by Ball Score, a line of string. For example:  


```
1.2wW6.2b34
```
## Output description
Individual scores of batsman that have played and number of extras. For example:


```
P1: 7  
 P2: 2  
 P3: 9  
 Extras: 2
```
Explanation : P1 hits a 1, P2 a dot ball, P2 hits a 2, Wide, P2 is Out (P3 in place on P2), P3 hits a 6, P3 a dot ball, New Over (P1 on strike), P1 hits a 2, Bye (P3 on strike), P3 hits a 3, P1 hits a 4  

## Challenge input

```
WWWWWWWWWW  
1..24.w6
```
# Finally
Have a good challenge idea?

Consider submitting it to /r/dailyprogrammer_ideas

(/r/dailyprogrammer_ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [351 [Easy] Cricket Scoring](https://www.reddit.com/r/dailyprogrammer/comments/7x81yg/20180213_challenge_351_easy_cricket_scoring/
)
