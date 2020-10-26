---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [99 (intermediate) (Unemployment map of the United States)](https://www.reddit.com/r/dailyprogrammer/comments/101mi5/9172012_challenge_99_intermediate_unemployment/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

A little while ago we took advantage of a very useful blank map hosted at Wikipedia. The advantage of this map is that it is very easy to assign different colors to each state (for details on how to do this, see the previous problem). We only had some silly fun with it, but it can also obviously be very useful in visualizing information about the country. 

(http://www.reddit.com/r/dailyprogrammer/comments/yj38u/8202012_challenge_89_difficult_coloring_the/)
(http://en.wikipedia.org/wiki/File:Blank_US_Map.svg)
(http://www.reddit.com/r/dailyprogrammer/comments/yj38u/8202012_challenge_89_difficult_coloring_the/)
Here is a text-file with unemployment data for all US states for each month from January 1980 to June 2012, stored in CSV format. The first column is the dates, then each column is the data for each state (the order of which is detailed in the headers). I got this information from the Federal Reserve Bank of St. Louis FRED database, which has excellent API access (good work, St. Louis Fed!). 

(https://gist.github.com/3740029)
(http://en.wikipedia.org/wiki/Comma-separated_values)
(http://research.stlouisfed.org/fred2/)
Using this table, make a program that can draw a map of unemployment across the United States at a given date. For instance, here is a map of unemployment for July 2005. As you can see, I edited the map slightly, adding a scale to the left side and a header that includes the date. You can do that too if you wish, but it is not necessary in any way. 

(http://i.imgur.com/O4LP2.png)
Your map doesn't need to look anything like mine. You can experiment with different colors and different styles. I selected the colors linearly based on unemployment, but you may want to use a different function to select colors, or perhaps color all states within a certain range the same (so that all states with 0%-2% are the same color, as are the states with 2%-4%, 4%-6%, etc). Experiment and see what you like. 

Create a map which shows unemployment for February 1995.


----
## **DISCLAIMER**
This prompt has been adapted from [99 [intermediate] (Unemployment map of the United States)](https://www.reddit.com/r/dailyprogrammer/comments/101mi5/9172012_challenge_99_intermediate_unemployment/
)
