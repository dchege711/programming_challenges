#_______________________________________________________________________________

import urllib.request as urllib2
import lxml
from bs4 import BeautifulSoup
import sys
import os

#_______________________________________________________________________________

baseURL = "https://www.reddit.com"

#_______________________________________________________________________________

outputFile = open('myURLS.txt', 'w')

def scrapeFromURL():
    count = 0
    hdr = { 'User-Agent' : 'Good Programmer' }
    # Set up resources to fetch the webpages
    with open(sys.argv[1], 'r') as listOfURLs:
        for url in listOfURLs:
            # currentURL = listOfURLs.readline()
            print(url)
            req = urllib2.Request(url, headers = hdr)
            currentPage = urllib2.urlopen(req).read()
            # Scrape the contacts to target .md file
            count += scrapeHTML(currentPage)
    print(str(count), "URLs found!")

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
    myButtons = mySoup.findAll('ul', class_ = 'flat-list buttons')
    print(len(myButtons))
    localCount = 0
    for button in myButtons:
        myFirst = button.find('li', class_ = 'first')
        aTag = myFirst.find('a')
        outputFile.write(aTag['href'] + "\n")
        if localCount % 10 == 0:
            print(aTag['href'])
        localCount += 1
    return localCount


#_______________________________________________________________________________

if __name__ == "__main__":
    scrapeFromURL()
    # scrapeLocalHTML()
