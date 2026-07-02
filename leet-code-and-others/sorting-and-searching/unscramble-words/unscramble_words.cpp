#include <string>
#include <vector>
#include <unordered_map>
#include <algorithm>

#include "unscramble_words.h"

namespace unscramble_words {

void PrintTo(const UnscrambledPair& pair, std::ostream* os) {
    *os << "\nUnscrambledPair{scrambled=" << pair.scrambled_word
        << "; unscrambled=";
    for (const auto& word : pair.unscrambled_words)
        *os << word << ",";
    *os << "}";
}

std::vector<UnscrambledPair> unscramble(
        const std::vector<std::string>& scrambled, const std::vector<std::string>& word_list) {
    // Pre-process `word_list` into a lookup table keyed by sorted anagrams.
    std::unordered_map<std::string, std::vector<std::string>> word_list_index{};
    word_list_index.reserve(word_list.size());
    for (const auto& word : word_list) {
        std::string k = word;
        std::ranges::sort(k);
        word_list_index[k].push_back(word);
    }

    // Iterate through `scrambled` and locate pairs.
    std::vector<UnscrambledPair> pairs{};
    pairs.reserve(scrambled.size());
    for (const auto& word : scrambled) {
        std::string k = word;
        std::ranges::sort(k);

        const auto it = word_list_index.find(k);
        if (it == word_list_index.end())
            continue;

        pairs.emplace_back(UnscrambledPair{word, it->second});
    }
    return pairs;
}

}
