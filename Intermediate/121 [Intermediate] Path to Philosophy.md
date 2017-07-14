# [121 (Intermediate) Path to Philosophy](https://www.reddit.com/r/dailyprogrammer/comments/1b3ka1/032713_challenge_121_intermediate_path_to/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

#  (Intermediate): Path to Philosophy
(#IntermediateIcon)
Clicking on the first link in the main text of a Wikipedia article not
in parentheses or italics, and then repeating the process for subsequent
articles, usually eventually gets you to the Philosophy article. As of
May 26, 2011, 94.52% of all articles in Wikipedia lead eventually to
the article Philosophy. The rest lead to an article with no wikilinks
or with links to pages that do not exist, or get stuck in
loops. Here's a Youtube video demonstrating this phenomenon.

(http://www.youtube.c%0Aom/watch?v=vehDe2lSptU)
Your goal is to write a program that will find the path from a given
article to the Philosophy article by following the first link (not in
parentheses, italics or tables) in the main text of the given article. Make
sure you have caching implemented from the start so you only need to
fetch each page once.

You will then extend the program to do a depth-first search in search
of the Philosophy article, backtracking if you get stuck and quitting
only when you know there is no such path. The last thing you
will do is generalise it to do a DFS towards any goal article.

Hint: Yes, there is a Wikipedia API. Feel free to use it.

The original formulation of this problem is found in the alternative
text to XKCD: Extended Mind.

(http://www.youtube.com/watch?v=vehDe2lSptU)
Author: nagasgura

# Formal Inputs & Outputs
## Input Description
Two strings, both which are names of existing Wikipedia articles (in
the Wikipedia language of your choice).

## Output Description
A path of Wikipedia articles, each linked from the previous one, that
leads from the start article to the end article.

You choose the output datastructure yourself, or print to standard-out.

# Sample Inputs & Outputs
## Sample Input
## Sample Output
(/r/dailyprogrammer_ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [121 [Intermediate] Path to Philosophy](https://www.reddit.com/r/dailyprogrammer/comments/1b3ka1/032713_challenge_121_intermediate_path_to/
)
