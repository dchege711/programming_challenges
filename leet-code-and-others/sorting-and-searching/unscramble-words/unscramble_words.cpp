#include <string>
#include <vector>
#include <map>
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
        std::vector<std::string> scrambled, std::vector<std::string> word_list) {
    std::map<std::string, std::vector<std::string>> word_list_index{};
    for (const auto& word : word_list) {
        std::string k = word;
        std::ranges::sort(k);
        if (word_list_index.contains(k)) {
            word_list_index.at(k).push_back(word);
        } else {
            word_list_index[k] = {word};
        }
    }

    std::vector<UnscrambledPair> pairs{};
    for (const auto& word : scrambled) {
        std::string k = word;
        std::ranges::sort(k);

        if (!word_list_index.contains(k))
            continue;

        pairs.emplace_back(UnscrambledPair{word, word_list_index[k]});
    }
    return pairs;
}

}
