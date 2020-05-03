---
draft: true
----

# [222 (Intermediate) Simple Stream Cipher](https://www.reddit.com/r/dailyprogrammer/comments/3chvxy/20150708_challenge_222_intermediate_simple_stream/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
Stream ciphers like RC4 operate very simply: they have a strong psuedo-random number generator that takes a key and produces a sequence of psuedo-random bytes as long as the message to be encoded, which is then XORed against the plaintext to provide the cipher text. The strength of the cipher then depends on the strength of the generated stream of bytes - its randomness (or lack thereof) can lead to the text being recoverable.

(https://en.wikipedia.org/wiki/RC4)
# Challenge Inputs and Outputs
Your program should have the following components:

(https://en.wikipedia.org/wiki/Linear_congruential_generator)
An example use of this API might look like this (in Python):


```
key = 31337
msg = "Attack at dawn"
ciphertext = enc(msg, key)
# send to a recipient

# this is on a recipient's side
plaintext = dec(ciphertext, key)
```
At this point, plaintext should equal the original msg value. 


```
plaintext
```

```
msg
```

----
## **DISCLAIMER**
This prompt has been adapted from [222 [Intermediate] Simple Stream Cipher](https://www.reddit.com/r/dailyprogrammer/comments/3chvxy/20150708_challenge_222_intermediate_simple_stream/
)
