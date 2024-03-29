---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [140 (Intermediate) Graph Radius](https://www.reddit.com/r/dailyprogrammer/comments/1tiz4z/122313_challenge_140_intermediate_graph_radius/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

#  (Intermediate): Graph Radius
(#IntermediateIcon)
In graph theory, a graph's radius is the minimum eccentricity of any vertex for a given graph. More simply: it is the minimum distance between all possible pairs of vertices in a graph.

(http://en.wikipedia.org/wiki/Graph_theory)
(http://en.wikipedia.org/wiki/Distance_(graph_theory))
As an example, the Petersen graph has a radius of 2 because any vertex is connected to any other vertex within 2 edges.

(http://en.wikipedia.org/wiki/Petersen_graph)
On the other hand, the Butterfly graph has a radius of 1 since its middle vertex can connect to any other vertex within 1 edge, which is the smallest eccentricity of all vertices in this set. Any other vertex has an eccentricity of 2.

(http://en.wikipedia.org/wiki/Butterfly_graph)
# Formal Inputs & Outputs
## Input Description
On standard console input you will be given an integer N, followed by an Adjacency matrix. The graph is not directed, so the matrix will always be reflected about the main diagonal.

(http://en.wikipedia.org/wiki/Adjacency_matrix)
(http://en.wikipedia.org/wiki/Main_diagonal)
## Output Description
Print the radius of the graph as an integer.

# Sample Inputs & Outputs
## Sample Input

```
10
0 1 0 0 1 1 0 0 0 0
1 0 1 0 0 0 1 0 0 0
0 1 0 1 0 0 0 1 0 0
0 0 1 0 1 0 0 0 1 0
1 0 0 1 0 0 0 0 0 1
1 0 0 0 0 0 0 1 1 0
0 1 0 0 0 0 0 0 1 1
0 0 1 0 0 1 0 0 0 1
0 0 0 1 0 1 1 0 0 0
0 0 0 0 1 0 1 1 0 0
```
## Sample Output

```
2
```

----
## **DISCLAIMER**
This prompt has been adapted from [140 [Intermediate] Graph Radius](https://www.reddit.com/r/dailyprogrammer/comments/1tiz4z/122313_challenge_140_intermediate_graph_radius/
)
