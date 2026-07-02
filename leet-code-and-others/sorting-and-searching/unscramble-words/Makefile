CXX := clang++
CXXFLAGS := -std=c++20 -Wall -Wextra
TARGET := unscramble_words
SRCS := unscramble_words.cpp file_utils.cpp

.PHONY: all run clean

all: $(TARGET)

$(TARGET): $(SRCS) file_utils.h
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(SRCS)

run: $(TARGET)
	./$(TARGET) scrambled.txt wordlist.txt

clean:
	rm -f $(TARGET)
