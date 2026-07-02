// Small collection of file-reading helpers shared across C++ solutions.

#ifndef FILE_UTILS_H
#define FILE_UTILS_H

#include <string>
#include <string_view>
#include <vector>

namespace utils {

// Reads whitespace-trimmed, non-empty lines from `file_path` into a vector.
// Throws `std::runtime_error` if the file cannot be opened.
std::vector<std::string> read_lines_from_file(const std::string& file_path);

// Reads whitespace-trimmed, non-empty lines from `file_path`, splits by
// `pattern`, and then returns the result. Throws `std::runtime_error` if the
// file cannot be opened.
std::vector<std::vector<std::string>> read_delimited_lines_from_file(
    const std::string& file_path, const std::basic_string_view<char> pattern);

}  // namespace utils

#endif  // FILE_UTILS_H
