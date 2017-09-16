'''
Scrapes data from r/dailyprogrammer and presents it in markdown
'''

#_______________________________________________________________________________

import urllib.request as urllib2
import lxml
from bs4 import BeautifulSoup
import sys
import os

#_______________________________________________________________________________

def scrapeFromURL():
    # Required to make multiple requests
    hdr = { 'User-Agent' : 'bot by /u/_Zagan_' }
    count = 0   # For informational purposes

    with open(sys.argv[1], 'r') as listOfURLs:
        for url in listOfURLs:
            print(str(count), url)

            # Set up resources to fetch the webpage
            req = urllib2.Request(url, headers = hdr)
            currentPage = urllib2.urlopen(req).read()

            # Scrape the contacts to target .md file
            scrapeHTML(currentPage, url)

            count += 1
    print(str(count), "docs created!")

#_______________________________________________________________________________

def scrapeLocalHTML():
    with open(sys.argv[1], 'r') as fileToRead:
        # Read in HTML Doc from command line
        myHTMLDoc = fileToRead.read()
        scrapeHTML(myHTMLDoc, "local_file")

#_______________________________________________________________________________

def scrapeHTML(myHTMLDoc, url):
    # Prepare Beautiful Soup object from the HTML file
    mySoup = BeautifulSoup(myHTMLDoc, "lxml")

    # Extract the title of the prompt
    try:
        title = mySoup.title.string.split("#")[1]
    except IndexError:
        title = mySoup.title.string
    try:
        title = title.split(":")[0].strip()
    except IndexError:
        title = title.strip()

    # Necessary to avoid messing up the file path in OSX
    title = title.replace("/", " ")

    # Prepare document for writing the data to
    pathAndFileName = getPathAndName(title)
    outputFile = open(pathAndFileName, 'w')

    # Write the document's title
    writeTitle(outputFile, title, url)

    # Extract the prompt
    try:
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
        writeDisclaimer(outputFile, title, url)

    # Some of the URLs don't contain programming prompts
    except AttributeError:
        print("Error", title)

#_______________________________________________________________________________

def getPathAndName(title):

    # Place the file in the correct folder
    fileName = title + ".md"
    if '[Easy]' in title or '[easy]' in title:
        relativePath = "/Users/dchege711/Reddit_Daily_Programmer/Easy/"
    elif '[Intermediate]' in title or '[intermediate]' in title:
        relativePath = "/Users/dchege711/Reddit_Daily_Programmer/Intermediate/"
    elif '[Hard]' in title or '[hard]' in title:
        relativePath = "/Users/dchege711/Reddit_Daily_Programmer/Hard/"
    else:
        relativePath = "/Users/dchege711/Reddit_Daily_Programmer/Bonus/"

    # Return the file path
    print(relativePath)
    return os.path.join(relativePath, fileName)

#_______________________________________________________________________________

def writeTitle(outputFile, title, url):
    # Markdown won't let me hyperlink when I have nested square brackets
    title = title.replace("[", "(")
    title = title.replace("]", ")")

    # Write the title and diclaimer
    outputFile.write("# [" + title + "](" + url.strip() + ")\n\n")
    outputFile.write("For the original " +
                    "[r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/)"
                     + " post and discussion, click the link in the title.\n\n")

#_______________________________________________________________________________

def writeCode(outputFile, item):
    outputFile.write("\n```\n")
    for descendant in item.descendants:
        text = descendant.strip()
        if text != "":
            outputFile.write(text)
    outputFile.write("\n```\n")

#_______________________________________________________________________________

def writeHeading(outputFile, header, level):
    for i in range(level):
        outputFile.write("#")
    outputFile.write(" ")
    outputFile.write(''.join(header.findAll(text=True)))
    outputFile.write("\n")

#_______________________________________________________________________________

def writeLink(outputFile, aTag):
    outputFile.write("(")
    outputFile.write(aTag['href'])
    outputFile.write(")\n")

#_______________________________________________________________________________

def writeParagraph(outputFile, pTag):
    outputFile.write(''.join(pTag.findAll(text=True)))
    outputFile.write("\n\n")

#_______________________________________________________________________________

def writeTable(outputFile, table):
    outputFile.write("\n|")

    headers = table.findAll('th')
    numberOfCols = len(headers)

    for headerItem in headers:
         outputFile.write(headerItem.text + "|")
    underLineTable(outputFile, numberOfCols)

    outputFile.write("\n|")
    tableBody = table.find('tbody')
    for row in tableBody.findAll('tr'):
        for cell in row.findAll('td'):
             outputFile.write(cell.text + "|")
        underLineTable(outputFile, numberOfCols)
        outputFile.write("\n|")

#_______________________________________________________________________________

def underLineTable(outputFile, numberOfCols):
    outputFile.write("\n|")
    for i in range(numberOfCols):
        outputFile.write(" --- |")

#_______________________________________________________________________________

def writeDisclaimer(outputFile, title, url):
    outputFile.write("\n----\n")
    outputFile.write("## **DISCLAIMER**\n")
    outputFile.write("This prompt has been adapted from [" +
                    title + "](" + url + ")\n")
#_______________________________________________________________________________

if __name__ == "__main__":
    scrapeFromURL()
    # scrapeLocalHTML()

#_______________________________________________________________________________
