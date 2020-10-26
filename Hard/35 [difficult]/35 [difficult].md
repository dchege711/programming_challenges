---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [35 (difficult)](https://www.reddit.com/r/dailyprogrammer/comments/rr5rq/432012_challenge_35_difficult/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

The objective of this exercise is to maintain a list of Strings in memory that support undo and redo.  Write a program that allows the user to add, edit, delete, undo, and redo entries in a list.  You must be able to undo and redo everything you've done during the execution of this program.  After each command is run, always print out the list (unless you're doing this in a UI).  Before writing any code, first think about how to write add, edit, and remove with undo and redo in mind.  If there are no submissions to this post, I'll reply with some hints.

Sample Run:

Enter command ('A'dd, 'E'dit, 'D'elete, 'U'ndo, 'R'edo): A

Enter text to add:  Venus

Venus

Enter command ('A'dd, 'E'dit, 'D'elete, 'U'ndo, 'R'edo): A

Enter text to add:  Mars

Venus

Mars

Enter command ('A'dd, 'E'dit, 'D'elete, 'U'ndo, 'R'edo): U

Venus

Enter command ('A'dd, 'E'dit, 'D'elete, 'U'ndo, 'R'edo): U

Enter command ('A'dd, 'E'dit, 'D'elete, 'U'ndo, 'R'edo): R

Venus

Enter command ('A'dd, 'E'dit, 'D'elete, 'U'ndo, 'R'edo): R

Venus

Mars

Enter command ('A'dd, 'E'dit, 'D'elete, 'U'ndo, 'R'edo): A

Enter text to add:  Saturn

Venus

Mars

Saturn

Enter command ('A'dd, 'E'dit, 'D'elete, 'U'ndo, 'R'edo): E

Enter index to edit:  1

Enter text to edit:  Earth

Venus

Earth

Saturn

Enter command ('A'dd, 'E'dit, 'D'elete, 'U'ndo, 'R'edo): U

Venus

Mars

Saturn

Enter command ('A'dd, 'E'dit, 'D'elete, 'U'ndo, 'R'edo): R

Venus

Earth

Saturn

Enter command ('A'dd, 'E'dit, 'D'elete, 'U'ndo, 'R'edo): D

Enter index to delete:  2

Venus

Earth

Enter command ('A'dd, 'E'dit, 'D'elete, 'U'ndo, 'R'edo): U

Venus

Earth

Saturn

Enter command ('A'dd, 'E'dit, 'D'elete, 'U'ndo, 'R'edo): R

Venus

Earth


----
## **DISCLAIMER**
This prompt has been adapted from [35 [difficult]](https://www.reddit.com/r/dailyprogrammer/comments/rr5rq/432012_challenge_35_difficult/
)
