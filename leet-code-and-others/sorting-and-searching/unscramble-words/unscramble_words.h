#ifndef LEET_CODE_AND_OTHERS_SORTING_AND_SEARCHING_UNSCRAMBLE_WORDS_UNSCRAMBLE_WORDS_H
#define LEET_CODE_AND_OTHERS_SORTING_AND_SEARCHING_UNSCRAMBLE_WORDS_UNSCRAMBLE_WORDS_H

#include <ostream>
#include <string>
#include <vector>

namespace unscramble_words
{

    struct UnscrambledPair {
        std::string scrambled_word;
        std::vector<std::string> unscrambled_words;

        auto operator<=>(const UnscrambledPair&) const = default;

        friend void PrintTo(const UnscrambledPair& pair, std::ostream* os);
    };

    std::vector<UnscrambledPair> unscramble(
        const std::vector<std::string>& scrambled,
        const std::vector<std::string>& word_list);

}  // namespace unscramble_words

#endif  // LEET_CODE_AND_OTHERS_SORTING_AND_SEARCHING_UNSCRAMBLE_WORDS_UNSCRAMBLE_WORDS_H
