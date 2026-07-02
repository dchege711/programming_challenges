#include <string_view>
#include <algorithm>

#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "file_utils.h"
#include "unscramble_words.h"

using unscramble_words::UnscrambledPair;
using unscramble_words::unscramble;

namespace {
    std::vector<UnscrambledPair> read_pairings(const std::string& file_path)
    {
        using std::operator""sv;

        constexpr auto pattern{","sv};
        const std::vector<std::vector<std::string>> raw_pairings =
            utils::read_delimited_lines_from_file(file_path, pattern);

        std::vector<UnscrambledPair> pairings;
        for (const auto& raw_pairing : raw_pairings)
        {
            pairings.emplace_back(
                UnscrambledPair{
                    raw_pairing.front(),
                    {raw_pairing.begin() + 1, raw_pairing.end()}});
        }
        return pairings;
    }
}

TEST(UnscrambleWordsTest, SampleInput) {
    const std::vector<std::string> scrambled = utils::read_lines_from_file("data/scrambled.txt");
    const std::vector<std::string> word_list = utils::read_lines_from_file("data/wordlist.txt");
    std::vector<UnscrambledPair> pairings = unscramble(scrambled, word_list);
    std::vector<UnscrambledPair> expected_pairings = read_pairings("data/unscrambled.txt");
    std::ranges::sort(expected_pairings);
    EXPECT_THAT(pairings, ::testing::WhenSorted(expected_pairings));
}
