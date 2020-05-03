---
draft: true
----

# [135 (Intermediate) De Morgan's Law](https://www.reddit.com/r/dailyprogrammer/comments/1qira9/111213_challenge_135_intermediate_de_morgans_law/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

#  (Intermediate): De Morgan's Law
(#IntermediateIcon)
De Morgan's Law is a neat law of Boolean propositional logic, helpful in every-day programming. It helps with inverting a boolean expression to get the opposite expression. Wolfram Mathworld has a much more correct definition using set theory, being equivalent to Boolean expressions.

(http://en.wikipedia.org/wiki/De_Morgan's_laws)
(http://mathworld.wolfram.com/deMorgansLaws.html)
(http://en.wikipedia.org/wiki/Boolean_algebra)
That's important since programming is all about Boolean-expressions! If you want to invert some "if" conditional code (that is, to compute the test for the "else" fall-through case), you need to apply the Law's two transformations:

An interpretation of these rules, helpful for applying it, is that you first apply the not-operator on all expressions, then replace all ands with ors, and vice-versa. This gets much more tricky and complex when order of operations come in play with parentheses (nested expressions).

Your goal is to take a C-like language's Boolean expression, and apply De Morgan's Law on it. You may choose to simplify the resulting expression as much as possible for epic bonus poinst; consider reading into Karnaugh maps as one approach. "Simplified" is measured in the least-amount of variables and operators required (not counting parentheses).

(http://en.wikipedia.org/wiki/Karnaugh_map)
# Formal Inputs & Outputs
## Input Description
The grammar of this C-like language is English-language space-delimited words, uses parentheses for nested expressions, with variables strictly being lower-case alpha-numeric. The reserved key-word for logical-and is "AND", with logical-or "OR", and logical-not "NOT". The expression will be in Infix-notation.

(http://en.wikipedia.org/wiki/Infix_notation)
Note that the "NOT" is a unary operator, meaning it always applies first to variables on the right of itself, or to a full expression. The expression "NOT a OR b" is equivalent to "(NOT a) OR b", but not "NOT (a or b)".

## Output Description
Given the expression, print it's inverse using the same grammar and given variables. Simplify the expression for epic bonus points.

# Sample Inputs & Outputs
## Sample Inputs

```
a
NOT a
a AND b 
NOT a AND b 
NOT (a AND b)
NOT (a OR b AND C) OR NOT(a AND NOT b)
```
## Sample Outputs

```
NOT a
a
NOT a or NOT b 
a OR NOT b 
a AND b
a AND NOT b
```

----
## **DISCLAIMER**
This prompt has been adapted from [135 [Intermediate] De Morgan's Law](https://www.reddit.com/r/dailyprogrammer/comments/1qira9/111213_challenge_135_intermediate_de_morgans_law/
)
