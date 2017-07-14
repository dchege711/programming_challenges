# [179 (Hard) Traveller Game Part 2 (Torchlight)](https://www.reddit.com/r/dailyprogrammer/comments/2g7ucz/9122014_challenge_179_hard_traveller_game_part_2/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description:
For today's challenge you must do the Intermediate Traveller Game challenge from wednesday. If you have already done it then you have a head start.

(http://www.reddit.com/r/dailyprogrammer/comments/2g1c80/9102014_challenge_179_intermediate_roguelike_the/)
We will modify our Traveller game by adding Torch light. Seeing the whole map is too easy. If you are limited in what you can see then you have a tougher time planning your moves.

You will modify your game the following ways.

# Examples:
Here are 3 examples of how the torchlight should work. 


```
Full Sight
    %%%%%%%%%%
    %..$.....%
    %......$.%
    %...@....%
    %....$...%
    %.$......%
    %%%%%%%%%%

    Torch Level 3
       %
      $..
     .....
    ...@...
     ...$.
      ...
       %     

    Full Sight (corner case)
    %%%%%%%%%%
    %@.$.....%
    %......$.%
    %........%
    %....$...%
    %.$......%
    %%%%%%%%%%

    Torch Level 3
    %%%%
    %@.$.
    %...
    %..
    .

    Full Sight (Barrier case)
    %%%%%%%%%%
    %..$.....%
    %.%%...$.%
    %...@....%
    %.%%%%%%.%
    %.$......%
    %%%%%%%%%%

    Torch Level 3
       %
       ..
     %%...
    ...@...
     %%%%%
```
# Harder:
Torches have a power of 5 instead of 3 -- every 2 Steps the Torch degenerates in power to 4 then 3 then 2 then 1 then none. In the room you will random place other "T" for torches or a light source which will refresh your torch power by +2 up to a max of 10. Again your Torch view will degenerate by 1 every 2 steps used (so if you can gain more than 5 torch power up to 10 but then it will degenerate 10-9-8 etc)

You will add 10 random pit traps. If the hero ends in the pit trap they die and game is over. 


----
## **DISCLAIMER**
This prompt has been adapted from [179 [Hard] Traveller Game Part 2 (Torchlight)](https://www.reddit.com/r/dailyprogrammer/comments/2g7ucz/9122014_challenge_179_hard_traveller_game_part_2/
)
