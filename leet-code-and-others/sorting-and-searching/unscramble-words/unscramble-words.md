---
date: 2020-05-03
domains:
- en.cppreference.com
- www.reddit.com
local_url: http://localhost:1313/computer-science/programming-challenges/leet-code-and-others/sorting-and-searching/unscramble-words/unscramble-words/
title: Unscrambling Words
---

## Problem Statement

Write a program that will take these scrambled words from `scrambled.txt`, and
compare them against `wordlist.txt` to unscramble them, e.g., `sleewa`
unscrambles to `weasel`. {{% cite DailyProgrammer03 %}}

## Solution

Pre-process `wordlist.txt` into a dictionary keyed by the sorted version of the
anagram, e.g., `weasel` is keyed under `aeelsw`. To support multiple anagrams,
the value should be a list of all strings from `wordlist` that share the sorted
anagram. Iterating through `scrambled.txt` becomes a lookup operation keyed on
the sorted string.

<details>
<summary>Implementation: Building a Word List Index</summary>

{{< readfile
  file="/content/computer-science/programming-challenges/leet-code-and-others/sorting-and-searching/unscramble-words/unscramble_words.cpp"
  highlight="cpp" >}}

</details>

## Notes

`operator[]` is an inserting access, i.e., `map[k].push_back(word)` is
equivalent to:

```cpp
if (k not in map) {
    map[k] = empty vector;
}
map[k].push_back(word);
```

... which is fine for a write path for "create if missing". However, if reading,
`map[k]` creates a new entry in the map on what was supposed to be a read-only
operation. Compare this to `map.at(k)`, which throws `std::out_of_range` if `k`
is not in `map`. {{% cite cppReferenceMapIndexOperator %}} {{% cite
cppReferenceMapAt %}}

Avoid the double lookup from contains-then-fetch, e.g.,

```cpp
if (!word_list_index.contains(k))
  continue;

pairs.emplace_back(UnscrambledPair{word, word_list_index[k]});
```

... by using `std::map<Key,T,Compare,Allocator>::find`, e.g.,

```cpp
const auto it = word_list_index.find(k);
if (it == word_list_index.end())
  continue;

pairs.emplace_back(UnscrambledPair{word, it->second});
```

## References

1. {{< citation
  id="DailyProgrammer03"
  title="[2/11/2012] challenge #3 [difficult] : r/dailyprogrammer"
  url="https://www.reddit.com/r/dailyprogrammer/comments/pkwgf/2112012_challenge_3_difficult/"
  accessed="2020-05-03" >}}

1. {{< citation
  id="cppReferenceMapIndexOperator"
  title="std::map<Key,T,Compare,Allocator>::operator[] - cppreference.com"
  url="https://en.cppreference.com/cpp/container/map/operator_at"
  accessed="2026-07-02" >}}

1. {{< citation
  id="cppReferenceMapAt"
  title="std::map<Key,T,Compare,Allocator>::at - cppreference.com"
  url="https://en.cppreference.com/cpp/container/map/at"
  accessed="2026-07-02" >}}
