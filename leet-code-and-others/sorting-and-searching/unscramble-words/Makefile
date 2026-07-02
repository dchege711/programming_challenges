CXX := clang++
CXXFLAGS := -std=c++20 -Wall -Wextra

GTEST_PREFIX := $(shell brew --prefix googletest)
GTEST_FLAGS := -I$(GTEST_PREFIX)/include -L$(GTEST_PREFIX)/lib

FILE_UTILS_TEST := file_utils_test
FILE_UTILS_TEST_SRCS := file_utils_test.cpp file_utils.cpp

UNSCRAMBLE_WORDS_TEST := unscramble_words_test
UNSCRAMBLE_WORDS_TEST_SRCS := unscramble_words_test.cpp unscramble_words.cpp file_utils.cpp

.PHONY: all test clean

all: test

$(FILE_UTILS_TEST): $(FILE_UTILS_TEST_SRCS) file_utils.h
	$(CXX) $(CXXFLAGS) $(GTEST_FLAGS) -o $(FILE_UTILS_TEST) $(FILE_UTILS_TEST_SRCS) \
		-lgtest -lgtest_main -lpthread

$(UNSCRAMBLE_WORDS_TEST): $(UNSCRAMBLE_WORDS_TEST_SRCS) unscramble_words.h file_utils.h
	$(CXX) $(CXXFLAGS) $(GTEST_FLAGS) -o $(UNSCRAMBLE_WORDS_TEST) $(UNSCRAMBLE_WORDS_TEST_SRCS) \
		-lgmock -lgmock_main -lgtest -lpthread

test: $(FILE_UTILS_TEST) $(UNSCRAMBLE_WORDS_TEST)
	./$(FILE_UTILS_TEST)
	./$(UNSCRAMBLE_WORDS_TEST)

clean:
	rm -f $(FILE_UTILS_TEST) $(UNSCRAMBLE_WORDS_TEST)
