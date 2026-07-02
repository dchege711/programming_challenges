// Small collection of file-reading helpers shared across C++ solutions.

#ifndef FILE_UTILS_H
#define FILE_UTILS_H

#include <string>
#include <vector>

// Reads whitespace-trimmed, non-empty lines from `file_path` into a vector.
// Throws std::runtime_error if the file cannot be opened.
std::vector<std::string> read_lines_from_file(const std::string& file_path);

#endif  // FILE_UTILS_H
