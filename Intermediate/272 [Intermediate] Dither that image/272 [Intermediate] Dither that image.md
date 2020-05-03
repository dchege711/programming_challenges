---
draft: true
----

# [272 (Intermediate) Dither that image](https://www.reddit.com/r/dailyprogrammer/comments/4paxp4/20160622_challenge_272_intermediate_dither_that/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
Dithering is the intentional use of
noise to reduce the error of compression. If you start with a color image
and want to reduce it to two colors (black and white) the naive approach is
to threshold the image. However, the results are usually terrible.

(https://en.wikipedia.org/wiki/Dither)
(http://i.imgur.com/kjWn2Q1.png)
(http://i.imgur.com/RDOMCfg.png)
One of the most popular dithering algorithms is
Floyd-Steinberg.
When a pixel is thresholded, the error (difference) between the original value
and the converted value is carried forward into nearby pixels.

(https://en.wikipedia.org/wiki/Floyd%E2%80%93Steinberg_dithering)
(http://i.imgur.com/w9DFOKS.png)
There are other approaches, such as
Ordered Dithering with a
Bayer Matrix.

(https://en.wikipedia.org/wiki/Ordered_dithering)
(http://i.imgur.com/mLKUyfn.png)
# Input
Your program will take a color or grayscale image as its input. You may choose
your input method appropriate to your language of choice. If you want to do it
yourself, I suggest picking a Netpbm
format, which is easy to read.

(https://en.wikipedia.org/wiki/Netpbm)
# Output
Output a two-color (e.g. Black and White) dithered image in your choice of
format. Again, I suggest picking a Netpbm format, which is easy to write.

# Notes
(http://www.tannerhelland.com/4660/dithering-eleven-algorithms-source-code/)
# Finally
Have a good challenge idea?
Consider submitting it to /r/dailyprogrammer_ideas

(/r/dailyprogrammer_ideas)
Thanks to /u/skeeto for this challenge idea

(/u/skeeto)
(https://www.reddit.com/r/dailyprogrammer_ideas/comments/4nt7rp)

----
## **DISCLAIMER**
This prompt has been adapted from [272 [Intermediate] Dither that image](https://www.reddit.com/r/dailyprogrammer/comments/4paxp4/20160622_challenge_272_intermediate_dither_that/
)
