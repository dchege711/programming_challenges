'''
Write a program that outputs the highest number that is lower or equal than the
square root of the given number, with the given number of decimal fraction digits.

Sample Input

0 7720.17
1 7720.17
2 7720.17

Sample Output
87
87.8
87.86

Restrictions
Use the method described here:
https://medium.com/i-math/how-to-find-square-roots-by-hand-f3f7cadf94bb

'''

import sys
import math

def inbuiltSquareRoot(precisionAsString, numberAsString):
    '''
    Gives back the square root truncated to the specified precision
    '''
    sqrt = math.sqrt(float(numberAsString))
    precision = int(precisionAsString)
    truncated = int(sqrt * (10 ** precision)) / (10 ** precision)
    return float('{:.{dp}f}'.format(truncated, dp = precision))

def manualSquareRoot(precisionAsString, numberAsString):
    '''
    Calculates the square root of number to the specified precision.
    Uses the manual square root method.
    '''


    numberAsString = padToMakeEven(numberAsString)
    # Recalculate the decimal point's index just in case the number was padded
    decimalIndex = numberAsString.find('.')
    originalLength = len(numberAsString)

    precision = int(precisionAsString)
    currentPrecision = -1
    index = 0
    # print(numberAsString + " (" + precisionAsString + ") ", end = "\t")

    # Try solving for the non-decimal part
    remnant = 0
    soln = 0
    divisor = 0
    while currentPrecision != precision:
        ceilingOfProduct = joinNumbers(remnant, numberAsString[index:index+2])

        if index >= originalLength - 1:
            ceilingOfProduct = joinNumbers(remnant, '00')

        # The case of the first index
        if index == 0:
            factor = 1
            while factor * factor <= ceilingOfProduct:
                factor += 1
            # Correct for the overshoot at the end of the loop
            factor += -1
            soln = joinNumbers(soln, factor)
            divisor = factor * 2
            remnant = ceilingOfProduct - (factor * factor)

        else:
            suffix = 9
            product = joinNumbers(divisor, suffix) * suffix
            while product > ceilingOfProduct:
                suffix += -1
                product = joinNumbers(divisor, suffix) * suffix
                remnant = ceilingOfProduct - product
            soln = joinNumbers(soln, suffix)
            divisor = joinNumbers(divisor, suffix) + suffix

        # print('s =', str(soln), 'd =', str(divisor), 'r =', str(remnant))

        index += 2
        if index == decimalIndex:
            index += 1

        if index > decimalIndex:
            currentPrecision += 1

    if currentPrecision != 0:
        soln = soln / (10 ** currentPrecision)

    return soln

def joinNumbers(prefixNumber, suffixNumber):
    return int(str(prefixNumber) + str(suffixNumber))

def padToMakeEven(numberAsString):
    '''
    Pads the number in case it has an odd number of digits
    '''
    decimalIndex = numberAsString.find('.')

    if decimalIndex != -1:
        numberOfChars = len(numberAsString)
        digitsBeforePoint = decimalIndex
        digitsAfterPoint = numberOfChars - decimalIndex - 1
        if (numberOfChars) % 2 == 0:
            if digitsBeforePoint % 2 != 0:
                numberAsString = '0' + numberAsString
            else:
                numberAsString = numberAsString + '0'

    else:
        numberOfDigits = len(numberAsString)
        if numberOfDigits % 2 != 0:
            # Pad with dp and 2 zeros to escape special cases
            numberAsString = '0' + numberAsString + '.00'
        else:
            numberAsString = numberAsString + '.00'

    return numberAsString

def main():
    with open(sys.argv[1], 'r') as inputNumbers:
        total = 0
        passed = 0
        for line in inputNumbers:
            line = line.split()
            precisionAsString = line[0]
            numberAsString = line[1]
            manualAns = manualSquareRoot(precisionAsString, numberAsString)
            inbuiltAns = inbuiltSquareRoot(precisionAsString, numberAsString)
            if manualAns != inbuiltAns:
                print(numberAsString + " (" + precisionAsString + ") : ", end = "\t")
                print("Manual: " + str(manualAns) + " != Inbuilt: " + str(inbuiltAns))
            else:
                passed += 1
            total += 1

    print("\n_______________________\n")
    print("Passed", str(passed), "/", str(total), "tests!")
    print("\n_______________________\n")

if __name__ == '__main__':
    main()
