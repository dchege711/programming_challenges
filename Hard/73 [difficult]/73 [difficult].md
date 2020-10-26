---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [73 (difficult)](https://www.reddit.com/r/dailyprogrammer/comments/w4m1r/762012_challenge_73_difficult/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

Huffman coding is a compression algorithm. Implementing it is a good occasion to work with queues, trees and bits.

(http://en.wikipedia.org/wiki/Huffman_coding)
Say we have a string of characters, and we want to transmit it over a network. To that end, we're gonna compress it.

The idea of the Huffman encoding is to replace each character by a bit sequence whose length depends on the frequency of occurrence of the character in the string: if a character occurs very often, we want to represent it by a very short bit sequence to avoid wasting space, but if appears only once or twice, it doesn't really matter if the bit sequence is long.

Exercise:

Write a function that takes a string and returns a Huffman tree, as described in the Wikipedia article.

Write an encoding function that takes a string and returns a sequence of bits that correspond to its Huffman encoding.

Write a decoding function that takes a sequence of bits and a Huffman tree, and reconstructs the original string.

Notice that you need the tree to decode a message. Bonus points if you figure out a way to encode the tree along with the bit sequence.

Also, don't let the gigantic introduction in the Wikipedia article discourage you, an algorithm is explained here. There's even a cute animation!

(http://en.wikipedia.org/wiki/Huffman_coding#Basic_technique)
(This challenge was posted to /r/dailyprogrammer_ideas by /u/wicked-canid -- thanks!)

(/r/dailyprogrammer_ideas)
(/u/wicked-canid)

----
## **DISCLAIMER**
This prompt has been adapted from [73 [difficult]](https://www.reddit.com/r/dailyprogrammer/comments/w4m1r/762012_challenge_73_difficult/
)
