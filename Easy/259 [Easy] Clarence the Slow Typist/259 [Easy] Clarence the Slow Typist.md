---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [259 (Easy) Clarence the Slow Typist](https://www.reddit.com/r/dailyprogrammer/comments/4bc3el/20160321_challenge_259_easy_clarence_the_slow/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
Clarence is a data entry clerk who works at an internet service provider. His job is to manually enter the IP addresses of all of the ISP's customers into the database. He does this using a keypad which has the following layout:


|1|2|3|
| --- | --- | --- |
|4|5|6|
| --- | --- | --- |
|7|8|9|
| --- | --- | --- |
|.|0||
| --- | --- | --- |
|
```
1
```

```
2
```

```
3
```

```
4
```

```
5
```

```
6
```

```
7
```

```
8
```

```
9
```

```
.
```

```
0
```
The distance between the centre of horizontally or vertically adjacent keys is exactly one centimetre. For instance, the distance between the centres of 3 and 9 would be two centimetres. The distance between the centres of 3 and 5 would be sqrt 2cm. The Pythagoras theorem is sufficient to calculate the distance between any two keys.


```
3
```

```
9
```

```
3
```

```
5
```
Clarence, as you might expect from one who works in an ISP, uses a very slow and inefficient system of typing. He uses a single finger and searches for the key, and then moves his finger to the key, then presses it, and repeats for all of the digits in the number. You might know of this style as the "eagle search system" since the finger searches above the keyboard for the correct key before plunging down for the keypress, like an eagle plunging down for a kill.

For example, here is how Clarence would type out the number 7851:


```
7851
```

```
7
```

```
8
```

```
5
```

```
1
```
Therefore the total distance that Clarence moved his finger to type in 7851 is 1 + 1 + sqrt 2 which is about 3.41cm.


```
7851
```

```
1 + 1 + sqrt 2
```
Your task is to write a program that calculates the distance Clarence must move his finger to type in arbitrary IP addresses.

# Formal Inputs and Outputs
## Input Description
Input is a string that will be in the form

().().().()


```
().().().()
```
where each () is an integer in the range 0 - 999. This represents the IP address that Clarence must type in. An example input might be:


```
()
```

```
0
```

```
999
```

```
219.45.143.143
```
I would also like to point out that inputs such as 0.42.42.42 or 999.999.999.999 are still valid inputs, despite the fact that they are invalid IP addresses. So you don't need to include any IP address verification code in your program.


```
0.42.42.42
```

```
999.999.999.999
```
## Output Description
Output the distance that Clarence must move his finger in order to type in the specified IP address. Round answers to two decimal places where needed, and use the cm unit in your output. The output for the example input is 27.38cm (1 + sqrt 8 + sqrt 5 + 2 + 1 + sqrt 5 + 3 + 1 + sqrt 5 + sqrt 13 + 3 + 1 + sqrt 5).


```
cm
```

```
27.38cm
```
# Credit
This challenge was suggested by /u/katyai. If you have any challenge ideas please share them on /r/dailyprogrammer_ideas and there's a good chance we'll use them!

(/u/katyai)
(/r/dailyprogrammer_ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [259 [Easy] Clarence the Slow Typist](https://www.reddit.com/r/dailyprogrammer/comments/4bc3el/20160321_challenge_259_easy_clarence_the_slow/
)
