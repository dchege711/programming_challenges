'''
My solution to Challenge #263: Help Eminem win his rap battle.

'''
#_______________________________________________________________________________

import sys
import pygtrie
import re
from difflib import SequenceMatcher as sequence

#_______________________________________________________________________________

class RhymeMaster:

    def __init__(self, syllableTypesFile, wordListFile):
        self.syllableTypes = self.fillSyllableTypes(syllableTypesFile)
        self.wordToSounds, self.soundsToWords = self.populateTries(wordListFile)

    #___________________________________________________________________________

    def fillSyllableTypes(self, inputFile):
        syllableTypes = {}
        with open(inputFile, 'r') as myFile:
            for line in myFile:
                items = line.split()
                syllableTypes[items[0]] = items[1]

        return syllableTypes
    #___________________________________________________________________________

    def populateTries(self, inputFile):

        soundsToWords = pygtrie.StringTrie(separator = '/')
        wordToSounds = pygtrie.CharTrie()
        index = 0
        with open(inputFile, 'r') as myFile:
            for line in myFile:
                items = line.split()
                word = items[0]

                reversePhonetics = list(reversed(items[1::]))
                reversePhoneticsString = '/'.join(reversePhonetics)

                soundsToWords[reversePhoneticsString] = word
                wordToSounds[word] = reversePhonetics

                index += 1

        return wordToSounds, soundsToWords

    #___________________________________________________________________________

    def getRhymingWords(self, word):
        reversePhoneticsArray = self.wordToSounds[word]
        # We only care about rhyming past the last vowel sound
        vowelIndex = 0
        for phonetic in reversePhoneticsArray:
            nonStressedPhonetic = re.split('(\d+)', phonetic)[0]
            if self.syllableTypes[nonStressedPhonetic] == 'vowel':
                break
            vowelIndex += 1

        relevantPhonetics = '/'.join(reversePhoneticsArray[0:vowelIndex+1])
        results = self.soundsToWords[relevantPhonetics:]
        return self.rankByCommonSyllables(word, results)[1:11] # Because it self-matches

    #___________________________________________________________________________

    def rankByCommonSyllables(self, word, results):
        wordRevPhonetic = self.wordToSounds[word]
        wordLen = len(wordRevPhonetic)
        wordsAndMatches = []

        count = 0

        for result in results:
            resultRevPhonetic = self.wordToSounds[result]
            resultLen = len(resultRevPhonetic)

            if resultLen < wordLen:
                maxIndex = resultLen
            else:
                maxIndex = wordLen

            numMatches = 0

            for i in range(maxIndex):
                if wordRevPhonetic[i] != resultRevPhonetic[i]:
                    break
                else:
                    numMatches += 1

            wordsAndMatches.append((result, numMatches))

            # if count % 100 == 0:
            #    print(wordRevPhonetic, 'vs', resultRevPhonetic, '=', numMatches)

            count += 1

        wordsAndMatches = list(reversed(sorted(wordsAndMatches, key = lambda item: item[1])))
        return wordsAndMatches

    #___________________________________________________________________________

    def getRhymeChain(self, word, limit = 6):
        listOfRhymes = [word]
        nextWord = self.getRhymingWords(word)[0][0]
        listOfRhymes.append(nextWord)

        for i in range(limit):
            if i % 2 != 0:
                phoneticArray = list(reversed(self.wordToSounds[nextWord]))
            else:
                phoneticArray = self.wordToSounds[nextWord]

            print(i, ':', phoneticArray)
            vowelIndex = self.getSyllableIndex(phoneticArray)
            relevantPhonetics = '/'.join(phoneticArray[0:vowelIndex+1])
            matches = self.soundsToWords[relevantPhonetics:]
            try:
                nextWord = self.rankByCommonSyllables(nextWord, matches)[0][0]
                listOfRhymes.append(nextWord)
            except KeyError as e:
                #print(e)
                #print(nextWord, ':', relevantPhonetics)

        return listOfRhymes

    def getSyllableIndex(self, phoneticArray):
        vowelIndex = 0
        for phonetic in phoneticArray:
            nonStressedPhonetic = re.split('(\d+)', phonetic)[0]
            if self.syllableTypes[nonStressedPhonetic] == 'vowel':
                break
            vowelIndex += 1
        return vowelIndex


    def peek(self, query, isPhonetic = True):
        if isPhonetic:
            return self.soundsToWords[query]
        else:
            return self.wordToSounds[query]

#_______________________________________________________________________________

if __name__ == '__main__':
    test = RhymeMaster('phonemes_description.txt', 'pronunciations.txt')
    # rhymes = test.getRhymingWords('AGANBEGYAN')
    # for rhyme in rhymes:
    #     print(rhyme[0])

    chain = test.getRhymeChain('SOLUTION')
    for word in chain:
        print(word, end = ' ... ')
    print()

    # print(test.peek('ACTUALIZE', isPhonetic = False))
    # print(test.peek('Z/AY2/L/AH0/UW2/CH/K/AE1'))
