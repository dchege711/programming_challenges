#include <string>
#include <vector>

#include "unscramble_words.h"

namespace unscramble_words {

void PrintTo(const UnscrambledPair& pair, std::ostream* os) {
    *os << "UnscrambledPair{scrambled=" << pair.scrambled_word
        << "; unscrambled=";
    for (const auto& word : pair.unscrambled_words)
        *os << word << ",";
    *os << "}";
}

std::vector<UnscrambledPair> unscramble(
        std::vector<std::string> scrambled, std::vector<std::string> word_list) {
    return {};
}

}
