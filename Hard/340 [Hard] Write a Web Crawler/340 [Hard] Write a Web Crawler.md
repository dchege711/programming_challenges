# [340 (Hard) Write a Web Crawler](https://www.reddit.com/r/dailyprogrammer/comments/7dlaeq/20171117_challenge_340_hard_write_a_web_crawler/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
Most of us are familiar with web spiders and crawlers like GoogleBot - they visit a web page, index content there, and then visit outgoing links from that page. Crawlers are an interesting technology with continuing development. 

(https://en.wikipedia.org/wiki/Web_crawler)
Web crawlers marry queuing and HTML parsing and form the basis of search engines etc. Writing a simple crawler is a good exercise in putting a few things together. Writing a well behaved crawler is another step up. 

For this challenge you may use any single shot web client you wish, e.g. Python's httplib or any of a number of libcurl bindings; you may NOT use a crawling library like Mechanize or whatnot. You may use an HTML parsing library like BeautifulSoup; you may NOT use a headless browser like PhantomJS. The purpose of this challenge is to tie together fetching a page, reassembling links, discovering links and assembling them, adding them to a queue, managing the depth of the queue, and visiting them in some reasonable order - while avoiding duplicate visits. 


```
httplib
```

```
libcurl
```

```
BeautifulSoup
```
Your crawler MUST support the following features:

Optional features include HTTPS support, support for robots.txt, support for domains to which you restrict the crawler, and storing results (for example how wget does so). 


```
robots.txt
```

```
wget
```
Be careful with what you crawl! Don't get yourself banned from the Internet. I highly suggest you crawl a local server you control as you may trigger rate limits and other mechanisms to identify unwanted visitors.


----
## **DISCLAIMER**
This prompt has been adapted from [340 [Hard] Write a Web Crawler](https://www.reddit.com/r/dailyprogrammer/comments/7dlaeq/20171117_challenge_340_hard_write_a_web_crawler/
)
