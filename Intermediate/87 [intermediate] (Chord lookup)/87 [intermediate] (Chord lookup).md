---
draft: true
----

# [87 (intermediate) (Chord lookup)](https://www.reddit.com/r/dailyprogrammer/comments/y0z3y/8102012_challenge_87_intermediate_chord_lookup/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

For this challenge, your task is to write a program that takes a musical chord name from input (like Gm7) and outputs the notes found in that chord (G A# D F). If you're no musician, don't worry -- the progress is quite simple. The first thing you need to know about is the 12 notes of the chromatic scale:


```
Gm7
```

```
G A# D F
```

```
C C# D D# E F F# G G# A A# B
```
The intervals between two notes is expressed in semitones. For example, there are three semitones between the D and the F on this scale. Next, you'll need to know about the different kinds of chords themselves:


```
D
```

```
F
```

|chord|symbol|tones|
| --- | --- | --- |
||
| --- | --- | --- |
|major|(nothing)|[0, 4, 7]|
| --- | --- | --- |
|minor|m|[0, 3, 7]|
| --- | --- | --- |
|dom. 7th|7|[0, 4, 7, 10]|
| --- | --- | --- |
|minor 7th|m7|[0, 3, 7, 10]|
| --- | --- | --- |
|major 7th|maj7|[0, 4, 7, 11]|
| --- | --- | --- |
|To find out the notes in a chord, take the base note, then select the tones from the chromatic scale relative to the numbers in the list of tone intervals. For example, for F7, we look up the chord:


```
F7
```

```
7 → dom. 7th → [0, 4, 7, 10]
```
Then we step [0, 4, 7, 10] semitones up from F in the scale, wrapping if necessary:


```
[0, 4, 7, 10]
```

```
F
```

```
[F+0, F+4, F+7, F+10] → [F, A, C, D#]
```
Those are the notes in our chord.

If you know a thing or two about music theory: for extra credit, tweak your program so that it...

outputs the chords "correctly", using b and bb and x where necessary


```
b
```

```
bb
```

```
x
```
supports more complex chords like A9sus4 or Emadd13.


```
A9sus4
```

```
Emadd13
```
(Bad submission timing, and I have to go right now -- expect [easy] and [difficult] problems tomorrow. Sorry!)


----
## **DISCLAIMER**
This prompt has been adapted from [87 [intermediate] (Chord lookup)](https://www.reddit.com/r/dailyprogrammer/comments/y0z3y/8102012_challenge_87_intermediate_chord_lookup/
)
