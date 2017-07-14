# [288 (Hard) Adjacent Numbers problems](https://www.reddit.com/r/dailyprogrammer/comments/58n2ca/20161021_challenge_288_hard_adjacent_numbers/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
You start with an empty grid of size m-by-m. Your goal is to fill it with numbers 1 through 9, so that the total sum of all numbers in the grid is the greatest.

## Rules
The grid fill rules are as follows:

# Formal Inputs and Outputs
## Input
The input consists of a positive integer representing size "m" of an m-by-m grid, e.g.:


```
grid(3)
```
## Output
The output consists of characters which represent a filled grid as per above rules, with an optimal solution (maximum total sum). The output format is a string of integers representing each row, with rows separated by line breaks (same format as the example solutions given below).

Below are example outputs for input:


```
grid(3)
```
Illegal solution:


```
111
222
333
```
Because the bottom "3"s must each be adjacent to both a "2" and a "1", yet they are only adjacent to a "2".

Legal but suboptimal solution:


```
123
321
123
```
In above example, each "3" is adjacent to a "2" and a "1", and each "2" is adjacent to a 1. However, the sum of the grid is 18, which is less than the maximum possible to achieve in a 3x3 grid.

Legal and optimal solution:


```
424
313
424
```
Each 4 is adjacent to a "3", "2", and "1"; each "3" is adjacent to a "2" and 1", and each "2" is adjacent to a "1". The sum of the above grid is 27, which is a maximum achievable sum in a 3x3 grid.

### Tips
## Bonus
Generalize this problem to an m-by-n grid. In this case, the input will be two digits "m" and "n", representing the width and height respectively, and the output would be a filled m-by-n grid. For example, input:


```
grid(3,2)
```
Could produce an optimal solution like:


```
313
424
```
# Credit
This challenge was submitted by /u/GeneReddit123, many thanks! If you have a challenge idea, please share it in /r/dailyprogrammer_ideas and there's a good chance we'll use it.

(/u/GeneReddit123)
(/r/dailyprogrammer_ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [288 [Hard] Adjacent Numbers problems](https://www.reddit.com/r/dailyprogrammer/comments/58n2ca/20161021_challenge_288_hard_adjacent_numbers/
)
