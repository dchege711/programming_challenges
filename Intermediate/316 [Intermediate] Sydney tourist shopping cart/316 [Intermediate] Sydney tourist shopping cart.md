---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [316 (Intermediate) Sydney tourist shopping cart](https://www.reddit.com/r/dailyprogrammer/comments/6d29om/20170524_challenge_316_intermediate_sydney/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
This challenge is to build a tourist booking engine where customers can book tours and activities around the Sydney.
Specially, you're task today is to build the shopping cart system. We will start with the following tours in our database.


|Id|Name|Price|
| --- | --- | --- |
|OH|Opera house tour|$300.00|
| --- | --- | --- |
|BC|Sydney Bridge Climb|$110.00|
| --- | --- | --- |
|SK|Sydney Sky Tower|$30.00|
| --- | --- | --- |
|As we want to attract attention, we intend to have a few weekly specials.

These promotional rules have to be as flexible as possible as they will change in the future. Items can be added in any order.

An object oriented interface could look like:


```
ShoppingCart sp = new ShopingCart(promotionalRules); 
sp.add(tour1);
sp.add(tour2);
sp.total();
```
Your task is to implement the shopping cart system described above. You'll have to figure out the promotionalRules structure, for example. 


```
promotionalRules
```
# Input Description
You'll be given an order, one order per line, using the IDs above. Example:


```
OH OH OH BC
OH SK
BC BC BC BC BC OH
```
# Output Description
Using the weekly specials described above, your program should emit the total price for the tour group. Example:


```
Items                 Total
OH, OH, OH, BC  =  710.00
OH, SK  = 300.00
BC, BC, BC, BC, BC, OH = 750
```
# Challenge Input

```
OH OH OH BC SK
OH BC BC SK SK
BC BC BC BC BC BC OH OH
SK SK BC
```
# Credit
This challenge was posted by /u/peterbarberconsult in /r/dailyprogrammer_ideas quite a while ago, many thanks! If you have an idea please feel free to share it, there's a chance we'll use it. 

(https://www.reddit.com/r/dailyprogrammer_ideas/comments/42n3zu/sydney_tourist_shopping_cart/)
(/u/peterbarberconsult)
(/r/dailyprogrammer_ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [316 [Intermediate] Sydney tourist shopping cart](https://www.reddit.com/r/dailyprogrammer/comments/6d29om/20170524_challenge_316_intermediate_sydney/
)
