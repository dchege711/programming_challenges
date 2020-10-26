---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [225 (Intermediate) Diagonal Maze](https://www.reddit.com/r/dailyprogrammer/comments/3f9o7k/20150731_challenge_225_intermediate_diagonal_maze/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

#  (Intermediate): Diagonal Maze
(#IntermediateIcon)
A maze can be represented using characters as follows:


```
+-+-+-+-+-+
  |       |
+ +-+-+ + +
| |     | |
+ + + + + +
|   | |   |
+-+-+ +-+-+
|     |   |
+ + +-+ + +
| |     |  
+-+-+-+-+-+
```
However, the exact same maze can also be represented diagonally using slashes, like this:


```
\
   / /\
  / /\ \
 /\   \ \
/  \/    \
\/   / / /
 \ \/\  /
  \   \/
   \/ /
    \
```
Your task today is to convert from the first format (cardinal) to the second (diagonal).

# Formal Inputs and Outputs
## Input Specification
You'll be given a number N on one line, followed by N further lines of input of a cardinal axis aligned maze, like so:


```
11
+-+-+-+-+-+
  |       |
+ +-+-+ + +
| |     | |
+ + + + + +
|   | |   |
+-+-+ +-+-+
|     |   |
+ + +-+ + +
| |     |  
+-+-+-+-+-+
```
The maze cells will not necessarily be one-by-one, so watch out!

## Output Description
Output the diagonal-ified maze, like the one shown above (same as in description).

# Sample Inputs and Outputs
## Example 1

```
16
+--+--+--+--+--+
      |     |  |
      |     |  |
+  +--+  +  +  +
|     |  |  |  |
|     |  |  |  |
+--+  +  +  +  +
|     |  |     |
|     |  |     |
+  +--+  +  +--+
|        |     |
|        |     |
+--+--+--+--+  +
|               
|               
+--+--+--+--+--+
```
### Output

```
\
           \
       /    \
      /      \
     /\   \  /\
    /  \   \/  \
   /       /    \
  /       /      \
 /\   \  /   /   /\
/  \   \/   /   /  \
\   \      /   /   /
 \   \    /   /   /
  \   \  /       /
   \   \/       /
    \   \   \  /
     \   \   \/
      \      /
       \    /
        \   
         \
```
## Example 2
### Input

```
17
+---+---+---+---+---+---+
                        |
                        |
                        |
+---+---+---+---+---+   +
                        |
                        |
                        |
+---+---+---+---+---+---+
|                        
|                        
|                        
+   +---+---+---+---+---+
|                        
|                        
|                        
+---+---+---+---+---+---+
```
### Output

```
\       
             \       
              \      
         \     \     
          \     \    
           \     \   
     /\     \     \  
    /  \     \     \ 
   /    \     \     \
  /      \     \     \       
 /        \     \     \       
/          \     \     \      
\     \     \     \     \     
 \     \     \     \     \    
  \     \     \     \     \   
   \     \     \     \     \  
    \     \     \     \     \ 
     \     \     \     \     \
      \     \     \          /
       \     \     \        /
        \     \     \      /
         \     \     \    /
          \     \     \  /
           \     \     \/
            \     \     
             \     \   
              \     \ 
               \     
                \   
                 \
```
# Finally
Got any cool challenge ideas? Submit them to /r/DailyProgrammer_Ideas!

(/r/DailyProgrammer_Ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [225 [Intermediate] Diagonal Maze](https://www.reddit.com/r/dailyprogrammer/comments/3f9o7k/20150731_challenge_225_intermediate_diagonal_maze/
)
