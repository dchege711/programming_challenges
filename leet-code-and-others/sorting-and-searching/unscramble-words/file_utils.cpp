#include "file_utils.h"

#include <fstream>
#include <stdexcept>

std::vector<std::string> read_lines_from_file(const std::string& file_path) {
    std::ifstream input_file(file_path);
    if (!input_file.is_open()) {
        throw std::runtime_error("Could not open file: " + file_path);
    }

    std::vector<std::string> lines;
    std::string line;
    while (std::getline(input_file, line)) {
        // Trim trailing carriage returns (e.g. CRLF line endings) and whitespace.
        const std::size_t last_non_whitespace = line.find_last_not_of(" \t\r\n");
        line.erase(last_non_whitespace == std::string::npos ? 0 : last_non_whitespace + 1);
        if (!line.empty()) {
            lines.push_back(line);
        }
    }

    return lines;
}
