---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [313 (Intermediate) PGM image manipulation](https://www.reddit.com/r/dailyprogrammer/comments/68zsoo/20170503_challenge_313_intermediate_pgm_image/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
Write a program that can perform the following operations on a given plain PGM image (described below):

It must also handle combinations of these, applied in order from left to right. For instance, HL means flip horizontal, and then rotate left. Note HL is different from LH, which is rotate left and then flip horizontal. The set of operations to perform will be given when your program is run.


```
HL
```

```
HL
```

```
LH
```
# Example input
(https://raw.githubusercontent.com/cosmologicon/problems/master/pgm/earth.pgm)
# Example outputs
(https://raw.githubusercontent.com/cosmologicon/problems/master/pgm/earth-R.pgm)
(https://raw.githubusercontent.com/cosmologicon/problems/master/pgm/earth-L.pgm)
(https://raw.githubusercontent.com/cosmologicon/problems/master/pgm/earth-H.pgm)
(https://raw.githubusercontent.com/cosmologicon/problems/master/pgm/earth-V.pgm)
(https://raw.githubusercontent.com/cosmologicon/problems/master/pgm/earth-HL.pgm)
# Input/output specification
Because plain PGM images are plain text, my recommended way of handling this is to read the input image from standard in, write the result to standard out, and use command-line arguments to specify what operations you want done. My solution is run from the command line like this:


```
python pgm-manip.py HRVRLLHL < input.pgm > output.pgm
```
However, you're not required to do it this way.

# Plain PGM image specification
The plain PGM format is a text-based grayscale image format that works in most image viewers, so you can open the file you output to check your work. Here's an example:

(http://netpbm.sourceforge.net/doc/pgm.html#plainpgm)

```
P2 4 3 100
0
100
100
0
100
33
33
100
0
100
100
0
```
The first line contains four things: the string P2, the image width in pixels (4 in this case), the image height (3 in this case), and the maximum pixel value (100 in this case). Each of the remaining lines (4x3, or 12 in this case) contains the value of a single pixel, where 0 is black and 100 is white, in order starting at the top left.


```
P2
```
As the link says, the PGM format allows any layout of these values, as long as there's whitespace between them. However, you may assume that the values are laid out as above. Also the PGM format allows comments with #, but you may assume there are no comments.


```
#
```
Previous r/dailyprogrammer challenges using related formats include Easy #172, Intermediate #172, and Easy #248. (Intermediate #171 was a related challenge with an ad-hoc image format.)

(/r/dailyprogrammer)
(https://www.reddit.com/r/dailyprogrammer/comments/2ba3g3/7212014_challenge_172_easy/?st=j28xacqj&sh=3ed7c3f7)
(https://www.reddit.com/r/dailyprogrammer/comments/2ba3nf/7232014_challenge172_intermediate_image_rendering/?st=j28xcp72&sh=fa3fe0d2)
(https://www.reddit.com/r/dailyprogrammer/comments/3zfajl/20160104_challenge_248_easy_draw_me_like_one_of/?st=j28xa8fg&sh=71cbfc6a)
(https://www.reddit.com/r/dailyprogrammer/comments/2avd5i/7162014_challenge_171_intermediate_zoom_rotate/?st=j28xgvcs&sh=682fcede)
Your language may have a handy library for manipulating images. If you really feel like it, you can use that, I guess, but the spirit of the challenge is to manipulate the pixel values yourself.

# Optional Bonus 1
Optimize your program so that it can efficiently handle many compound operations. I don't want to give away too much here, but for instance, the operation HRLVH is actually the same as simply V. So if you realize that, you don't need to apply each of the five operations separately. You can get away with one. In fact, there are only eight possible outputs from your program, no matter how many operations are requested.


```
HRLVH
```

```
V
```
If you do this right, then you should be able to run your program for thousands of operations, and it should only take slightly longer than if you run it for only one operation.

# Optional bonus 2
Also handle the following operations:

Some of these operations are "lossy", meaning that if you apply them, it may not be possible to get back to your exact original image. But try to make it so that E/S, B/D, and C/W are roughly opposites. This means that BD should, roughly speaking, get you back to your original image, the same way RL, HH, or NN does.


```
BD
```

```
RL
```

```
HH
```

```
NN
```
# Optional bonus 3
Also handle plain PPM images. These are similar to PGM's but they're in color, with three values for each pixel. Your same program should handle both PGM and PPM. You can tell which one it is because PGM's start with P2 and PPM's start with P3. Example input:

(http://netpbm.sourceforge.net/doc/ppm.html#plainppm)

```
P2
```

```
P3
```
(https://raw.githubusercontent.com/cosmologicon/problems/master/pgm/earth.ppm)
Ideally your program will handle both PGM and PPM in the same code path, with only small differences for the two formats, rather than two completely separate code paths.


----
## **DISCLAIMER**
This prompt has been adapted from [313 [Intermediate] PGM image manipulation](https://www.reddit.com/r/dailyprogrammer/comments/68zsoo/20170503_challenge_313_intermediate_pgm_image/
)
