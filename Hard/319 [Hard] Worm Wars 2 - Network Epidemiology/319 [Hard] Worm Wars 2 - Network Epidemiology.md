---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [319 (Hard) Worm Wars 2 - Network Epidemiology](https://www.reddit.com/r/dailyprogrammer/comments/6hm5j2/20170616_challenge_319_hard_worm_wars_2_network/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
This one builds on the previous challenge: malware propagation. But now we add a twist - network topology, specifically connectivity and congestion.

Real world network malware can't attack a host it can't connect to. That connection may be blocked due to a lack of connectivity between the host (e.g. not directly connected networks), or a congested pipe. Network connections get congested when they're flooded with traffic, forcing packet loss. 

For today's challenge, you're being asked to model a small network in which some malware has been introduced. Unlike the previous challenge, you have to traverse the network to reach all nodes. This more realistically mimics propagation where contact is required to propagate. Work with these assumptions:

# Challenge Input
You'll be given a lot of information for this one. First an integer on one line telling you how many networks to read. For each network specification you'll have a line telling you the network ID (a letter), the number of hosts in it (N), the number of infected hosts at time 0 (I). Then another integer telling you how many links to read. Then that many lines telling you what two networks connect and with what capacity in bytes per second (assume symmetric connections). Finally for the malware you'll be given some values on a line telling you the transition rates for S to I, I to R and S to R. Finally a line with a single integer, B, telling you the size of the malware propagation packet (assume UDP, so a single packet to infect). Example:


```
10
A 1000 1 
B 1000 0
C 1000 3
D 1000 0
E 1000 0
F 1000 1
G 1000 10
H 1000 0
I 1000 0
J 1000 90
10
A B 10000
B C 1000
C D 2000
D E 2000
D F 2000
D G 5000
D H 9000
D I 1000
D A 8000
D J 10000
0.01 0.01 0.015
256
```
# Challenge Input 2

```
15
A 4412 0
B 12035 5
C 11537 9
D 10873 15
E 7269 12
F 10989 19
G 9680 3
H 8016 14
I 5373 10
H 10738 18
J 1329 9
K 12168 0
L 9436 2
M 1769 0
N 7564 8
14
A B 1000
B C 1000
C D 1000
D E 1000
E F 1000
F J 1000
F G 1000
G K 1000
G H 1000
H I 1000
H L 1000
I E 1000
I M 1000
I N 1000
0.01 0.01 0.015
256
```
# Output Description
Your program can emit answers in a number of different ways:


----
## **DISCLAIMER**
This prompt has been adapted from [319 [Hard] Worm Wars 2 - Network Epidemiology](https://www.reddit.com/r/dailyprogrammer/comments/6hm5j2/20170616_challenge_319_hard_worm_wars_2_network/
)
