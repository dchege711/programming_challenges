// Unscrambling Words
//
// Reads a list of words (one per line) from a file such as `scrambled.txt`
// or `wordlist.txt`, and prints how many words were loaded from each.
//
// Build:  make
// Run:    make run   (or: ./unscramble_words scrambled.txt wordlist.txt)

#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

#include "file_utils.h"

int main(int argc, char* argv[]) {
    if (argc < 3) {
        std::cerr << "Usage: " << argv[0] << " <scrambled.txt> <wordlist.txt>\n";
        return 1;
    }

    try {
        const std::vector<std::string> scrambled_words = read_lines_from_file(argv[1]);
        const std::vector<std::string> word_list = read_lines_from_file(argv[2]);

        std::cout << "Loaded " << scrambled_words.size() << " scrambled words from "
                  << argv[1] << "\n";
        std::cout << "Loaded " << word_list.size() << " dictionary words from "
                  << argv[2] << "\n";

        // TODO: Unscramble each word in `scrambled_words` by matching it
        // against `word_list`, then sort the results by length.
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }

    return 0;
}
