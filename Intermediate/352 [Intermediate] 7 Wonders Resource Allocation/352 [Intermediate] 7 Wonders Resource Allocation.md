---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [352 (Intermediate) 7 Wonders Resource Allocation](https://www.reddit.com/r/dailyprogrammer/comments/7z8hrm/20180221_challenge_352_intermediate_7_wonders/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
In the board game 7 Wonders, there are four basic resources, which we'll abbreviate with letters: W for wood, B for brick, S for stone, and O for ore.

Resource cards let you produce one of your choice of resources. We'll use W/B to represent a resource card that can give you either 1 wood or 1 brick, but not both.

Given the resource cards W/B/S/O, W, S/B, and S, it is possible for you to produce 2 wood and 2 stone: use the first two cards to get wood, and the last two to get stone. However, with that same set of cards, it is impossible for you to produce 2 wood and 2 brick.

# Input
You'll be given a comma-separated sequence of cards inside of square brackets, with the features separated by a slash. Your target will be given as "Can you make ____?" with the list of resources to target, one per card. 

Note: in the game 7 Wonders, every card produces either 1, 2, or all 4 of the resources. But if you want a challenge, make your program support any number of resources instead of just W,B,S,O, and make your program accept arbitrary resource cards.

# Output
Whether it is possible to generate the desired resources, and if so, how.

# Example Inputs
With line breaks for clarity.


```
Cards [W/B/S/O, W, S/B, S]. Can you make WWSS?

Cards [W/B/S/O, S/O, W/S, W/B, W/B, W, B]. Can you make WWBSSOO?

Cards [A/B/D/E, A/B/E/F/G, A/D, A/D/E, A/D/E, B/C/D/G, B/C/E, B/C/E/F, 
B/C/E/F, B/D/E, B/D/E, B/E/F, C/D/F, C/E, C/E/F/G, C/F, C/F, D/E/F/G, 
D/F, E/G]. Can you make AABCCCCCCDDDEEEEFFGG?

Cards [A/C/G/K/L/O/R/S, A/D/H/I/M/Q, A/D/K/W/X, A/D/M/U/Z, A/E/J/M/T, 
A/G/H/I/M/R/T/Z, A/G/M/T/U, A/H/I/J/Q, B/C/Q/U/V, B/D/F/K/M/R/W/Y, 
B/F/P/T/U/W/Y, B/G/K/M/S/T/X/Y, C/E/F/I/K/N/O, D/E/G/J/M/Q/Z, D/G/I/R/Z, 
D/H/I/T/U, E/G/H/J/M/Q, E/G/H/J/Q/R/T/U, E/G/J/M/Z, E/H/I/Q/T/U/Z, 
E/J/O/S/V/X, F/G/H/N/P/V, F/G/N/P/R/S/Z, F/I/M/Q/R/U/Z, F/L/M/P/S/V/W/Y, 
G/H/J/M/Q]. Can you make ABCDEFGHIJKLMNOPQRSTUVWXYZ?
```
# Bonus
Make your program much faster than brute force.

# Credit
This challenge was submitted by /u/Lopsidation in /r/dailyprogrammer_ideas, many thanks! If you have a challenge idea please share it and there's a good chance we'll use it. 

(/u/Lopsidation)
(/r/dailyprogrammer_ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [352 [Intermediate] 7 Wonders Resource Allocation](https://www.reddit.com/r/dailyprogrammer/comments/7z8hrm/20180221_challenge_352_intermediate_7_wonders/
)
