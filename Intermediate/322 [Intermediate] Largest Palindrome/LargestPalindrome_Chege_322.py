'''
Given an integer input n, this program prints the largest integer that
is a palindrome and has two factors both of string length n, e.g.

An input 2 gives an output of 9009
Explanation: 9009 has factors 99 and 91

1   9
2   9009
3   906609
4   99000099
5   9966006699

'''
#_______________________________________________________________________________

import sys

#_______________________________________________________________________________

def getPalindrome(n):
    # Brute force:
    # 99x99, 99x98, 99x97, ... , 98x98, 98x97, 98x96,...
    # Inefficiency: We are not looking at the largest first

    # Inspection shows that the optimum traversal method is...
    #   99x99
    #     |
    #   99x98 --> 98x98
    #               |
    #             99x97 --> 98x97 --> 97x97
    #                                   |
    #                                 99x96 --> 98x96 --> 97x96 --> 96x96
    #

    numberOfFactors = int(n)
    highestFactor = (10 ** numberOfFactors) - 1
    lowestFactor = 10 ** (numberOfFactors - 1)
    factorOne = highestFactor
    factorTwo = highestFactor

    while factorOne >= lowestFactor and factorTwo >= lowestFactor:
        product = factorOne * factorTwo
        print(factorOne, "x", factorTwo, "=", product)
        if checkIfPalindrome(product):
            return str(product)
        if (factorOne == factorTwo):    # 99x99 --> 99x98, 97x97 --> 99x96
            factorTwo += -1
            factorOne = highestFactor
        elif (factorTwo < factorOne):
            factorOne += -1
    return None
#_______________________________________________________________________________

def checkIfPalindrome(candidate):
    '''
    Given an integer, this returns True if the integer is a palindrome.
    Returns False otherwise
    '''
    candidate = str(candidate)
    backCursor = len(candidate) - 1
    frontCursor = 0

    while (frontCursor <= backCursor):
        if (candidate[frontCursor] != candidate[backCursor]):
            return False
        frontCursor += 1
        backCursor += -1
    return True

#_______________________________________________________________________________

def main():
    # Maintain counter variables to keep track of tests
    count = 0
    passed = 0

    with open(sys.argv[1], 'r') as inputFile:
        for line in inputFile:
            items = line.split()
            myPalindrome = getPalindrome(items[0])
            # Note down any failed tests
            if myPalindrome != items[1]:
                print(str(myPalindrome), "should be", items[1])
            else:
                passed += 1
            count += 1

    # Print a summary of the tests
    print("\n____________________")
    print(str(passed), "/", str(count), "tests passed!")
    print("\n____________________")

#_______________________________________________________________________________

if __name__ == '__main__':
    main()
