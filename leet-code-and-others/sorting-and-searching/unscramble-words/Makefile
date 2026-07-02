CXX := clang++
CXXFLAGS := -std=c++20 -Wall -Wextra
TARGET := unscramble_words
SRCS := unscramble_words.cpp file_utils.cpp

GTEST_PREFIX := $(shell brew --prefix googletest)
TEST_TARGET := file_utils_test
TEST_SRCS := file_utils_test.cpp file_utils.cpp

.PHONY: all run test clean

all: $(TARGET)

$(TARGET): $(SRCS) file_utils.h
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(SRCS)

run: $(TARGET)
	./$(TARGET) scrambled.txt wordlist.txt

$(TEST_TARGET): $(TEST_SRCS) file_utils.h
	$(CXX) $(CXXFLAGS) -I$(GTEST_PREFIX)/include -L$(GTEST_PREFIX)/lib \
		-o $(TEST_TARGET) $(TEST_SRCS) -lgtest -lgtest_main -lpthread

test: $(TEST_TARGET)
	./$(TEST_TARGET)

clean:
	rm -f $(TARGET) $(TEST_TARGET)
