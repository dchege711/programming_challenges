'''
Scrapes data from r/dailyprogrammer and presents it in markdown
'''

#_______________________________________________________________________________

import urllib.request as urllib2
import lxml
from bs4 import BeautifulSoup
import sys

#_______________________________________________________________________________

#_______________________________________________________________________________

def scrapeFromURL():
    hdr = { 'User-Agent' : 'bot by /u/_Zagan_' }
    # Set up resources to fetch the webpages
    with open(sys.argv[1], 'r') as listOfURLs:
        for url in listOfURLs:
            # currentURL = listOfURLs.readline()
            print(url)
            req = urllib2.Request(url, headers = hdr)
            currentPage = urllib2.urlopen(req).read()
            # Scrape the contacts to target .md file
            scrapeHTML(currentPage)
    print("Done!")

#_______________________________________________________________________________

def scrapeLocalHTML():
    with open(sys.argv[1], 'r') as fileToRead:
        # Read in HTML Doc from command line
        myHTMLDoc = fileToRead.read()
        scrapeHTML(myHTMLDoc)

#_______________________________________________________________________________

def scrapeHTML(myHTMLDoc):
    # Prepare Beautiful Soup object from the HTML file
    mySoup = BeautifulSoup(myHTMLDoc, "lxml")

    # Extract the title of the prompt
    title = mySoup.title.string.split("#")[1]
    title = title.split(":")[0].strip()
    # Prepare document for writing the data to
    title = title + ".md"
    outputFile = open(title, 'w')
    print(title)
    outputFile.write("#" + title + "\n\n")

    # Extract the prompt
    promptFolder = mySoup.find('div', class_ = 'expando expando-uninitialized')
    prompt = promptFolder.find('div', class_ = 'md')

    # Format the markdown as needed
    for child in prompt.descendants:
        myTag = child.name
        if (myTag == 'code'):
            writeCode(outputFile, child)
        if (myTag == 'h1'):
            writeHeading(outputFile, child, 1)
        if (myTag == 'h2'):
            writeHeading(outputFile, child, 2)
        if (myTag == 'h3'):
            writeHeading(outputFile, child, 3)
        if myTag == 'a':
            writeLink(outputFile, child)
        if myTag == 'p':
            writeParagraph(outputFile, child)
        if myTag == 'table':
            writeTable(outputFile, child)

#_______________________________________________________________________________

def writeCode(outputFile, item):
    print("```")
    outputFile.write("\n```\n")
    for descendant in item.descendants:
        text = descendant.strip()
        if text != "":
            print(text)
            outputFile.write(text)
    print("```")
    outputFile.write("\n```\n")

#_______________________________________________________________________________

def writeHeading(outputFile, header, level):
    for i in range(level):
        print("#", end = "")
        outputFile.write("#")
    print(" ", end = "")
    outputFile.write(" ")
    print(''.join(header.findAll(text=True)))
    outputFile.write(''.join(header.findAll(text=True)))
    outputFile.write("\n")

#_______________________________________________________________________________

def writeLink(outputFile, aTag):
    print("(", end = "")
    outputFile.write("(")
    print(aTag['href'], end = "")
    outputFile.write(aTag['href'])
    print(")")
    outputFile.write(")\n")

#_______________________________________________________________________________

def writeParagraph(outputFile, pTag):
    print(''.join(pTag.findAll(text=True)))
    outputFile.write(''.join(pTag.findAll(text=True)))
    outputFile.write("\n\n")

def writeTable(outputFile, table):
    print()
    outputFile.write("\n|")
    print("|", end = "")

    headers = table.findAll('th')
    numberOfCols = len(headers)

    for headerItem in headers:
         print(headerItem.text, end = "|")
         outputFile.write(headerItem.text + "|")
    underLineTable(outputFile, numberOfCols)

    print("\n| ", end = "")
    outputFile.write("\n|")
    tableBody = table.find('tbody')
    for row in tableBody.findAll('tr'):
        for cell in row.findAll('td'):
             print(cell.text, end = "|")
             outputFile.write(cell.text + "|")
        underLineTable(outputFile, numberOfCols)
        print("\n| ", end = "")
        outputFile.write("\n|")

def underLineTable(outputFile, numberOfCols):
    print("\n| ", end = "")
    outputFile.write("\n|")
    for i in range(numberOfCols):
        print(" --- ", end = "|")
        outputFile.write(" --- |")
#_______________________________________________________________________________

if __name__ == "__main__":
    #scrapeFromURL()
    scrapeLocalHTML()

#_______________________________________________________________________________
