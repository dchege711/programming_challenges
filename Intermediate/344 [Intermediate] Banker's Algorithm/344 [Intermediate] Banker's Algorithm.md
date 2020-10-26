---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [344 (Intermediate) Banker's Algorithm](https://www.reddit.com/r/dailyprogrammer/comments/7jkfu5/20171213_challenge_344_intermediate_bankers/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description:
Create a program that will solve the banker’s algorithm.  This algorithm stops deadlocks from happening by not allowing processes to start if they don’t have access to the resources necessary to finish.  A process is allocated certain resources from the start, and there are other available resources.  In order for the process to end, it has to have the maximum resources in each slot.

## Example:

|Process|Allocation|Max|
| --- | --- | --- |
||A B C|A B C|
|P0|0   1   0|7   5   3|
|P1|2   0   0|3   2   2|
|P2|3   0   2|9   0   2|
|P3|2   1   1|2   2   2|
|P4|0   0   2|4   3   3|

Suppose there is 3, 3, 2 available at the start. P1 or P3 would be able to go first.  Let’s pick P1 for the example.  Next, P1 will release the resources that it held, so the next available would be 5, 3, 2.

# The Challenge:
Create a program that will read a text file with the banker’s algorithm in it, and output the order that the processes should go in.
An example of a text file would be like this:
```
3 3 2
0 1 0 7 5 3
2 0 0 3 2 2
3 0 2 9 0 2
2 1 1 2 2 2
0 0 2 4 3 3
```

And the program would print out:

```
P1, P4, P3, P0, P2
```

# Bonus:
Have the program tell you if there is no way to complete the algorithm.


----
## **DISCLAIMER**
This prompt has been adapted from [344 [Intermediate] Banker's Algorithm](https://www.reddit.com/r/dailyprogrammer/comments/7jkfu5/20171213_challenge_344_intermediate_bankers/)
