---

layout: default
title: Linear Feedback Shift Register (Intermediate)
date: 2018-01-22
type: writeup

---

<nav aria-label="Breadcrumb" class="breadcrumb">
    <ul>
        <li><a href="{{site.baseurl}}">Home</a></li>
        <li><a href="{{site.baseurl}}/intermediate_challenges">Intermediate Challenges</a></li>
        <li><span aria-current="page">{{page.title}}</span></li>
    </ul>
</nav>

## Main Challenge

[Link to original problem](https://www.reddit.com/r/dailyprogrammer/comments/7r17qr/20180117_challenge_347_intermediate_linear/)

### Description

In computing, a [linear-feedback shift register (LFSR)](https://en.wikipedia.org/wiki/Linear-feedback_shift_register) is a shift register whose input bit is a linear function of its previous state. The most commonly used linear function of single bits is exclusive-or (XOR). Thus, an LFSR is most often a shift register whose input bit is driven by the XOR of some bits of the overall shift register value.

The initial value of the LFSR is called the seed, and because the operation of the register is deterministic, the stream of values produced by the register is completely determined by its current (or previous) state. Likewise, because the register has a finite number of possible states, it must eventually enter a repeating cycle.

Your challenge today is to implement an LFSR in software.

#### Example Input

You'll be given a LFSR input on one line specifying the tap positions (0-indexed), the feedback function (XOR or XNOR), the initial value with leading 0s as needed to show you the bit width, and the number of clock steps to output. Example:

```text
[0,2] XOR 001 7
```

#### Example Output

Your program should emit the clock step and the registers (with leading 0s) for the input LFSR. From our above example:

```text
0 001
1 100
2 110
3 111
4 011
5 101
6 010
7 001
```

#### Challenge Input

```text
[1,2] XOR 001 7
[0,2] XNOR 001 7
[1,2,3,7] XOR 00000001 16
[1,5,6,31] XOR 00000000000000000000000000000001 16
```

#### Challenge Output

(Only showing the first two for brevity's sake.)

```text
0 001
1 100
2 010
3 101
4 110
5 111
6 011
7 001
```

```text
0 001
1 000
2 100
3 010
4 101
5 110
6 011
7 001
```

### My Solution

```java
{% include_relative 347_intermediate_linear_feedback_shift_register/LFSR347Hack.java %}
```

### Further Reading

* [Implementing LFSR using Logic](https://www.eetimes.com/document.asp?doc_id=1274550)
* [LFSR: Theory and Applications](http://homepages.cae.wisc.edu/%7Eece553/handouts/LFSR-notes.PDF)

### Bonus

Write a function that detects the periodicity of the LFSR configuration.

### Post-Mortem

I wrote this solution 20 months ago; a post-mortem is in order:

* I left print statements inside functions.
* I didn't close my `java.io.BufferedReader`
* I couldn't get [java.util.BitSet](https://docs.oracle.com/javase/8/docs/api/java/util/BitSet.html) working.
