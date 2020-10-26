---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [130 (Easy) Roll the Dies](https://www.reddit.com/r/dailyprogrammer/comments/1givnn/061713_challenge_130_easy_roll_the_dies/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

#  (Easy): Roll the Dies
(#EasyIcon)
In many board games, you have to roll multiple multi-faces dies.jpg) to generate random numbers as part of the game mechanics. A classic die used is the d20 (die of 20 faces) in the game Dungeons & Dragons. This notation, often called the Dice Notation, is where you write NdM, where N is a positive integer representing the number of dies to roll, while M is a positive integer equal to or grater than two (2), representing the number of faces on the die. Thus, the string "2d20" simply means to roll the 20-faced die twice. On the other hand "20d2" means to roll a two-sided die 20 times.

(http://en.wikipedia.org/wiki/File:Dice_(typical_role_playing_game_dice)
(http://en.wikipedia.org/wiki/Dice_notation)
Your goal is to write a program that takes in one of these Dice Notation commands and correctly generates the appropriate random numbers. Note that it does not matter how you seed your random number generation, but you should try to as good programming practice.

(http://en.wikipedia.org/wiki/Random_seed)
Author: nint22

# Formal Inputs & Outputs
## Input Description
You will be given a string of the for NdM, where N and M are describe above in the challenge description. Essentially N is the number of times to roll the die, while M is the number of faces of this die. N will range from 1 to 100, while M will range from 2 to 100, both inclusively. This string will be given through standard console input.

## Output Description
You must simulate the die rolls N times, where if there is more than one roll you must space-delimit (not print each result on a separate line). Note that the range of the random numbers must be inclusive of 1 to M, meaning that a die with 6 faces could possibly choose face 1, 2, 3, 4, 5, or 6.

# Sample Inputs & Outputs
## Sample Input

```
2d20
4d6
```
## Sample Output

```
19 7
5 3 4 6
```

----
## **DISCLAIMER**
This prompt has been adapted from [130 [Easy] Roll the Dies](https://www.reddit.com/r/dailyprogrammer/comments/1givnn/061713_challenge_130_easy_roll_the_dies/
)
