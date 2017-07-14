#317 [Hard] Poker Odds

# DESCRIPTION
Playing Texas Hold'em is a game about weighing odds. Every player is given two cards that only they can see. Then five cards are turned up on the table that everybody sees. The winner is the player with the best hand composed of five cards out of the seven available (the 5 on the table, and the two personal cards).

Your job is, given four hands of two cards, and the "flop" (three of the five cards that will be flipped up), calculate the odds every player has of getting the best hand.

# INPUT
You will be given 5 lines, the first line contains the three cards on the flop, the next four with the two-card hands of every player. written as [CardValue][CardSuit], with the values being, in order, A, 2, 3, 4, 5, 6, 7, 8, 9, 0, J, Q, K, A (Aces A may be high or low, just like real poker). The suits' corresponding symbols are the first letter of the suit name; Clubs = C; Spades = S; Diamonds = D; Hearts = H.


```
A
```
# OUTPUT
Four lines of text, writing...


```
[PlayerNum] : [Odds of Winning (rounded to 1 decimal point)] %
```
# SAMPLE INPUT

```
3D5C9C    
3C7H    
AS0S    
9S2D    
KCJC
```
# SAMPLE OUTPUT

```
1: 15.4%    
2: 8.8%    
3: 26.2%    
4: 49.6%
```
# NOTES
For those unfamiliar, here is the order of hand win priority, from best up top to worst at the bottom;

In the event that two people have the same hand value, whichever has the highest card that qualifies of that rank. ie; If you get a pair, the value of the pair is counted first, followed by high-card. If you have a full house, the value of the triplet is tallied first, the the pair.
  * Per se; two hands of 77820 and 83J77 both have pairs, of sevens, but then Person 2 has the higher "high card" outside the ranking, a J beats a 0.

If there is a chance of a tie, you can print that separately, but for this challenge, only print out the chance of them winning by themselves.

ALSO REMEMBER; There are 52 cards in a deck, there can't be two identical cards in play simultaneously.

# Credit
This challenge was suggested by /u/Mathgeek007, many thanks. If you have a suggestion for a challenge, please share it at /r/dailyprogrammer_ideas and there's a good chance we'll use it. 

(/u/Mathgeek007)
(/r/dailyprogrammer_ideas)
