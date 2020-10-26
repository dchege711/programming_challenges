---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [248 (Easy) Draw Me Like One Of Your Bitmaps](https://www.reddit.com/r/dailyprogrammer/comments/3zfajl/20160104_challenge_248_easy_draw_me_like_one_of/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

Let's build a basic paint program! Your task for today will be to create a basic paint program that can draw points, lines, and filled rectangles, then output an image file that many image viewers can read. But first, some background:

# Netpbm Formats
PNG, GIF, JPEG, and even BMP are all image formats that are way too complex for an [Easy] challenge. Instead, we are going to be using Netpbm formats. More specifically, we will be using the PPM format, which supports 24-bit RGB color. Here's how a .ppm file looks (courtesy of Wikipedia):

(https://en.wikipedia.org/wiki/Netpbm_format)

```
.ppm
```

```
P3
# The P3 means colors are in ASCII, then 3 columns and 2 rows,
# then 255 for max color, then RGB triplets
3 2
255
255   0   0     0 255   0     0   0 255
255 255   0   255 255 255     0   0   0
```
Each pixel in the image is represented with 3 integers (0-255) for its Red, Green, and Blue pixel values. The above .ppm file gets displayed as this (zoomed in). 


```
.ppm
```
(https://upload.wikimedia.org/wikipedia/commons/5/57/Tiny6pixel.png)
Everything is separated by whitespace, but what the whitespace is (and how much of it there is) doesn't matter. Comments (anything after a #) are also ignored. In other words, the following PPM file renders exactly the same image:


```
#
```

```
P3 3 2 255 255 0 0 0 255 0 0 0 255 255 255 0 255 255 255 0 0 0
```
Lastly, note that in image processing, pixels are indexed using (row, column) coordinates, counting up from (0, 0). Thus, in the image above, the pixel at (0, 2) is on row 0, column 2, which has the RGB value of 0 0 255, or in other words, is blue.


```
(row, column)
```

```
(0, 0)
```

```
(0, 2)
```

```
0 0 255
```
Now that that's out of the way, let's get to painting!

# Formal Input
Your input file will contain an X/Y size for an image to create, followed by a series of commands, each on its own line. The commands each start with point, line, or rect, followed by a RGB color, followed by whatever arguments the command needs. Here's a sample:


```
point
```

```
line
```

```
rect
```

```
5 3
point 0 0 255 0 0
line 100 100 100 0 2 2 4
rect 77 0 0 1 3 2 2
```
Breaking the file down line by line:


```
5 3
```

```
point
```

```
0 0 255
```

```
0 0
```

```
line
```

```
100 100 100
```

```
0 2
```

```
2 4
```

```
rect
```

```
77 0 0
```

```
1 3
```

```
2 2
```
The "unpainted" background can be assumed to be black (0 0 0).


```
0 0 0
```
# Formal Output
The output PPM file for the above example should look like this (more or less, spacing notwithstanding):


```
P3
5 3
255
0   0   255    0   0   0      100 100 100    0   0   0      0   0   0  
0   0   0      0   0   0      0   0   0      77  0   0      77  0   0  
0   0   0      0   0   0      0   0   0      77  0   0      77  0   0
```
And it should render like this (zoomed in).

(https://i.imgur.com/EaGSFdZ.png)
# Challenge Input

```
400 300
rect 0 0 255 0 0 300 400
line 255 255 255 0 0 299 399
line 255 255 255 299 0 0 399
rect 200 200 0 100 150 100 100
point 0 0 0 150 200
```
# Challenge Output
Actual output: https://raw.githubusercontent.com/fsufitch/dailyprogrammer/master/248_easy/sample2_tight.ppm

(https://raw.githubusercontent.com/fsufitch/dailyprogrammer/master/248_easy/sample2_tight.ppm)
Converted to PNG and posted to Imgur: https://i.imgur.com/nRmSoUf.png

(https://i.imgur.com/nRmSoUf.png)
# Big Challenge
Run these commands: https://raw.githubusercontent.com/fsufitch/dailyprogrammer/master/248_easy/sierpinsky.txt

(https://raw.githubusercontent.com/fsufitch/dailyprogrammer/master/248_easy/sierpinsky.txt)
You should get something like this: https://i.imgur.com/5F31DSE.png

(https://i.imgur.com/5F31DSE.png)
# Bonus Points
If you would like more of a challenge, implement the following commands:


```
bline <R> <G> <B> <row1> <col1> <row2> <col2>
```
(https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm)

```
circle <R> <G> <B> <centerRow> <centerCol> <radius>
```

```
ellipse <R> <G> <B> <centerRow> <centerCol> <radiusVertical> <radiusHorizontal>
```

```
fill <R> <G> <B> <row> <col>
```

```
smartfill <R> <G> <B> <row> <col> <tolerance>
```

```
sqrt( (r2-r1)^2 + (g2-g1)^2 + (b2-b1)^2)
```
# Resources
(https://convertio.co/ppm-png/)
(https://www.imagemagick.org/)
(https://www.gimp.org/)
Have any cool ideas for challenges? Come post them over in /r/dailyprogrammer_ideas!

(/r/dailyprogrammer_ideas)
Got feedback? We (the mods) would like to know how we're doing! Are the problems too easy? Too hard? Just right? Boring/exciting? Varied/same? Anything you would like to see us do that we're not doing? Anything we're doing that we should just stop? Come by this feedback thread and let us know!

(https://redd.it/3zgexx)

----
## **DISCLAIMER**
This prompt has been adapted from [248 [Easy] Draw Me Like One Of Your Bitmaps](https://www.reddit.com/r/dailyprogrammer/comments/3zfajl/20160104_challenge_248_easy_draw_me_like_one_of/
)
