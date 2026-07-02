#include <cstdlib>
#include <fstream>
#include <stdexcept>
#include <unistd.h>

#include <gtest/gtest.h>

#include "file_utils.h"

using utils::read_lines_from_file;

namespace {

// Writes `contents` to a uniquely named temporary file and returns its path.
std::string write_temp_file(const std::string& contents) {
    char path_template[] = "/tmp/file_utils_test_XXXXXX";
    const int fd = mkstemp(path_template);
    if (fd == -1) {
        throw std::runtime_error("Could not create temporary file");
    }
    close(fd);

    std::ofstream out(path_template);
    out << contents;
    out.close();
    return path_template;
}

}  // namespace

TEST(ReadLinesFromFileTest, ReturnsEachNonEmptyLine) {
    const std::string path = write_temp_file("mkeart\nsleewa\nedcudls\n");
    const std::vector<std::string> lines = read_lines_from_file(path);

    EXPECT_EQ(lines, (std::vector<std::string>{"mkeart", "sleewa", "edcudls"}));
    std::remove(path.c_str());
}

TEST(ReadLinesFromFileTest, SkipsBlankLines) {
    const std::string path = write_temp_file("foo\n\n\nbar\n");
    const std::vector<std::string> lines = read_lines_from_file(path);

    EXPECT_EQ(lines, (std::vector<std::string>{"foo", "bar"}));
    std::remove(path.c_str());
}

TEST(ReadLinesFromFileTest, TrimsTrailingWhitespaceAndCarriageReturns) {
    const std::string path = write_temp_file("foo \r\nbar\t\n");
    const std::vector<std::string> lines = read_lines_from_file(path);

    EXPECT_EQ(lines, (std::vector<std::string>{"foo", "bar"}));
    std::remove(path.c_str());
}

TEST(ReadLinesFromFileTest, ThrowsWhenFileDoesNotExist) {
    EXPECT_THROW(read_lines_from_file("this_file_does_not_exist.txt"), std::runtime_error);
}
