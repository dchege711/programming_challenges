---
draft: true
----

# [182 (Intermediate) The Data Collator from Jamaica](https://www.reddit.com/r/dailyprogrammer/comments/2i13a4/10012014_challenge_182_intermediate_the_data/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

#  (Intermediate): The Data Collator from Jamaica
(/IntermediateIcon)
Often, when given a set of data where one variable is associated with another, we want to find a general rule equating the two variables, with which you can find the closest appropriate match of one to the other.

Say, for example, we have performed an experiment determining the acceleration undergone by an object when subject to a force. Newton's 2nd Law of Motion dictates that F=ma - linking the variables F (force) and a (acceleration) by a constant m (mass of the object). If we performed the acceleration we may get the following values:

(https://en.wikipedia.org/wiki/Newton%27s_laws_of_motion#Newton.27s_second_law)

```
F
```

```
a
```

```
m
```

|F (N)|a (m s-2)|
| --- | --- |
|0.2|0.32|
| --- | --- |
|0.4|0.62|
| --- | --- |
|0.6|0.97|
| --- | --- |
|0.8|1.22|
| --- | --- |
|1|1.58|
| --- | --- |
|1.2|1.84|
| --- | --- |
|1.4|2.17|
| --- | --- |
|1.6|2.47|
| --- | --- |
|1.8|2.83|
| --- | --- |
|2|3.16|
| --- | --- |
|This data can be plotted to see the link between the 2 data sets. Here, F is on the horizontal and a is on the vertical axis.

(http://i.imgur.com/PdlTl6W.png)

```
F
```

```
a
```
To create a line of best-fit or trend line for this data, which looks like this, a number of methods can be used, such as the ever-present least squares method. For the purposes of this challenge, the trend line will always be linear, and thus the two data sets must be 

(http://i.imgur.com/W2aFGIx.png)
(https://en.wikipedia.org/wiki/Least_squares)
Your challenge is, given 2 data sets, draw the values on an appropriately-scaled graph (with axes) and find a suitable trend line fitting the data.

# Input and Output Description
## Input
The first line of input will be in the format:


```
<X>:<graph title>:<X label>:<Y label>
```
Following that will be precisely N further lines of input, in the format:


```
X:Y
```
Where X is the value to be plotted on the X-axis, and Y is the value to be plotted on the Y-axis.

# Output
The output is to be in the form of an image:

# Sample Input
I've created a data set for you to plot yourself.


```
20:Graph of I over V through a resistor:Voltage (V):Current (mA)
0.000:0.000
0.198:0.387
0.400:0.781
0.600:1.172
0.802:1.566
1.003:1.962
1.200:2.349
1.402:2.735
1.597:3.122
1.798:3.505
2.002:3.918
2.202:4.314
2.399:4.681
2.603:5.074
2.800:5.485
2.997:5.864
3.198:6.256
3.400:6.631
3.597:7.017
3.801:7.435
```
# Tips
Here are some tips to make the most of this /r/DailyProgrammer challenge.

(/r/DailyProgrammer)
Try and think of an algorithm or method to find the best-fit line yourself. There are plenty of ways out there, but as a member of /r/DailyProgrammer try and do it from scratch!

(/r/DailyProgrammer)
Half of the challenge here is drawing the graph yourself. For that reason it's best to pick a language here that supports graphical output. Using a premade graphing library defeats the point of this challenge so try and DIY.


----
## **DISCLAIMER**
This prompt has been adapted from [182 [Intermediate] The Data Collator from Jamaica](https://www.reddit.com/r/dailyprogrammer/comments/2i13a4/10012014_challenge_182_intermediate_the_data/
)
