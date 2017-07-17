# [224 (Hard) Langford strings](https://www.reddit.com/r/dailyprogrammer/comments/3efbfh/20150724_challenge_224_hard_langford_strings/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
A "Langford string of order N" is defined as follows: 

An example will make this clearer. These are the only two possible Langford strings of order 3:


```
BCABAC
CABACB
```
Notice that for both strings, the A's have 1 letter between them, the B's have two letters between them, and the C's have three letters between them. As another example, this is a Langford string of order 7:


```
DFAGADCEFBCGBE
```
It can be shown that Langford strings only exist when the order is a multiple of 4, or one less than a multiple of 4.

Your challenge today is to calculate all Langford strings of a given order.

# Formal inputs & outputs
## Inputs
You will be given a single number, which is the order of the Langford strings you're going to calculate.

## Outputs
The output will be all the Langford strings of the given order, one per line. The ordering of the strings does not matter. 

Note that for the second challenge input, the output will be somewhat lengthy. If you wish to show your output off, I suggest using a service like gist.github.com or hastebin and provide a link instead of pasting them directly in your comments.

(http://gist.github.com)
(http://hastebin.com)
# Sample input & output
## Input

```
3
```
## Output

```
BCABAC
CABACB
```
# Challenge inputs
## Input 1

```
4
```
## Input 2

```
8
```
# Bonus
For a bit of a stiffer challenge, consider this: there are more than 5 trillion different Langford strings of order 20. If you put all those strings into a big list and sorted it, what would the first 10 strings be?

# Notes
If you have a suggestion for a challenge, head on over to /r/dailyprogrammer_ideas and we might use it in the future!

(/r/dailyprogrammer_ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [224 [Hard] Langford strings](https://www.reddit.com/r/dailyprogrammer/comments/3efbfh/20150724_challenge_224_hard_langford_strings/
)
