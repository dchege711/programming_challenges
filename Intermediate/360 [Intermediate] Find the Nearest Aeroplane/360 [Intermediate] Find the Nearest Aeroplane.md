---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [360 (Intermediate) Find the Nearest Aeroplane](https://www.reddit.com/r/dailyprogrammer/comments/8i5zc3/20180509_challenge_360_intermediate_find_the/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
We want to find the closest airborne aeroplane to any given position in North America or Europe. To assist in this we can use an API which will give us the data on all currently airborne commercial aeroplanes in these regions.

OpenSky's Network API can return to us all the data we need in a JSON format.

(https://opensky-network.org/apidoc/rest.html)

```
https://opensky-network.org/api/states/all
```
From this we can find the positions of all the planes and compare them to our given position.

Use the basic Euclidean distance in your calculation. 

(https://en.wikipedia.org/wiki/Euclidean_distance)
# Input
A location in latitude and longitude, cardinal direction optional

An API call for the live data on all aeroplanes

# Output
The output should include the following details on the closest airborne aeroplane:


```
Geodesic distance
Callsign
Lattitude and Longitude
Geometric Altitude
Country of origin
ICAO24 ID
```
# Challenge Inputs
Eifel Tower:


```
48.8584 N
2.2945 E
```
John F. Kennedy Airport:


```
40.6413 N
73.7781 W
```
# Bonus
Replace your distance function with the geodesic distance formula, which is more accurate on the Earth's surface. 

(https://en.wikipedia.org/wiki/Great-circle_distance)
# Challenge Credit:
This challenge was posted by /u/Major_Techie, many thanks. Major_Techie adds their thanks to /u/bitfluxgaming for the original idea.

(/u/Major_Techie)
(/u/bitfluxgaming)

----
## **DISCLAIMER**
This prompt has been adapted from [360 [Intermediate] Find the Nearest Aeroplane](https://www.reddit.com/r/dailyprogrammer/comments/8i5zc3/20180509_challenge_360_intermediate_find_the/)
