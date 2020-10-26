---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [249 (Hard) Museum Cameras](https://www.reddit.com/r/dailyprogrammer/comments/41346z/20160115_challenge_249_hard_museum_cameras/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
You run a museum, and you have a small budget - but you have to protect the museum with cameras. Given some descriptions of rooms, can you organize the smallest number of cameras to view the whole room?

Some assumptions and other factors for you to work with:

# Input Description
You'll be given a row with a single number N that tells you how many points to read. Then on the next line you'll be given N points in a Cartesian coordinate space to draw the bounding box of the museum room. For example:


```
3
(0,0) (3,6) (6,0)
```
This translates to (pardon my ugly ASCII art) this triangle:


```
.                                       .
                                              / \
                            =>               /   \
                                            /     \
                                           /       \
                                          /         \
.             .                          .___________.
```
# Output Description
Your program should emit the position of the cameras needed to cover the area. From our example:


```
(0,0)
```
That's one possible solution (for this one any of the corners would have worked). 

If the shape has no solution, emit something like "The architect has no concept of security" because maybe they're collaborating with art theives. 

# Challenge Input
first room


```
4 
(0,0) (5,0) (5,6) (0,6)
```
second room


```
5
(0,0) (7,0) (7,3) (5,6) (0,6)
```
third room


```
13
(0,5) (2,8) (5,7) (9,6) (10,9) (13,10) (13,6) (17,6) (16,3) (13,1) (7,1) (5,3) (2,3)
```
# Notes
This is a classic computational geometry problem called the Art Gallery Problem. For some ideas on calculating 2d visibility from a top down map, click here 

(https://en.wikipedia.org/wiki/Art_gallery_problem)
(http://www.redblobgames.com/articles/visibility/)

----
## **DISCLAIMER**
This prompt has been adapted from [249 [Hard] Museum Cameras](https://www.reddit.com/r/dailyprogrammer/comments/41346z/20160115_challenge_249_hard_museum_cameras/
)
