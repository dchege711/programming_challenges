---
draft: true
----

# [309 (Hard) Patterns overlap](https://www.reddit.com/r/dailyprogrammer/comments/641zpj/20170407_challenge_309_hard_patterns_overlap/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

Taken from practice problem for google code jam (which starts tonight)

Input consists of 2 strings, where:


```
*
```

```
*
```
The challenge is to return True if there exists a substitution of *s in both strings that make the 2 strings identical.


```
True
```

```
*
```
Sample:


```
Shakes*e
S*speare
```
output:

True - 1st string can replace * with pear and 2nd string can replace * with hake


```
*
```

```
pear
```

```
*
```

```
hake
```
sample 2:


```
a*baa**ba**aa
*ca*b**a*baac
```
can be quickly determined false in that the first string cannot be made to end in c.


```
a*baa**ba**aa
*ca*b**a*baaa
```
True:  both strings can be made into acabaabaaa


```
acabaabaaa
```
Challenges:


```
bb*aaaaa*ba**
*baabb*b*aaaa

dnKeeuCCyHOnobnDYMGoXDdNWhTsaoedbPifJ*ki*wWfXjIUwqItTmGqtAItoNWpDeUnNCWgZsKWbuQxKaqemXuFXDylQubuZWhMyDsXvDSwYjui*LviGAEkyQbtR*cELfxiAbbYyJRGtcsoJZppINgJGYeZKGeWLbenBEKaoCgheYwOxLeFZJPGhTFRAjNn*
d*eeuCCyHOnobnDYMGoXDdNWhTsaoedbP*ijrwWfXjIUwqItTmGqtAItoNWpDeUnNCWgZs*WbuQxKaqemXuFXDylQubuZWhMyDsXvDSwYjuijkLviGAEkyQbtRUsncELfxiAbbYyJRG*soJZppINgJGYeZKGeWLbenBEKaoCghe*YwOxLeFZJPGhTFRAjNn

THAkZYrkUWgcTpZ*SsNQKsEnvdUveZxssEtCEQuoMqToJjMdCatMs*v*GyMlROpiIDUZyJjhwmjxFWpEwDgRLlLsJYebMSkwxEUvoDcLPLIwHY*GvoRhgcfkdsenObSjWGNYRDJAzRzavAGRoZZ*fDXIRlJkufqHDjLMJKEjLAkRRyQqTrUaWRIndSX
*THAkZYrkUWgcTpZSsNQKsEnvdUveZxssEtCEQuoMqToJjMdCatMsYa*nBvIFuGyMlROpiIDUZyJjh*FWpEwDgRLlLsJYebMSkw*oDcLPLIwHYbeBGvoRhgcfkdsenObSjWGNYRDJAzRzavAGRoZZvbEfDXIRlJkufqHDjLMJKEjLAkRRyQqTrU*aWRIndSX

jEAmXdDUtthXNLbIZFeWdiQPGEvyCEeLI**EyficABUH*YiSZRREvniDexKJSjLXMYfsw*YlbTSZBlYSecorJsWidfALQYzOdrKNrJZRdrQEDoyhPMYAfTiHZIuqGtEkKqYBzxtCOJhRYfZNSYNxRWFrfahlSLvdBTebrXDgGlZEqxRIvGhN*mfhLLSExNHaHLAZI
jEAmXdDUtthXNLbIZFeWdiQPGEvyCEeL**BUHYiSZRREvniDexKJSjLXMYfswlaYlbTSZBlYSecorJsWidfALQYzOdrKNrJZ*EDoyhPMYAfTiHZIuqGtEkKqYBzxtC*YfZNSYNxRWFrfahlSLvdBT*ebrXDgGlZEqxRIvGhNcmfhLLSExNHaHLAZI
```

----
## **DISCLAIMER**
This prompt has been adapted from [309 [Hard] Patterns overlap](https://www.reddit.com/r/dailyprogrammer/comments/641zpj/20170407_challenge_309_hard_patterns_overlap/
)
