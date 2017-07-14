# [322 (Hard) Static HTTP Server](https://www.reddit.com/r/dailyprogrammer/comments/6lti17/20170707_challenge_322_hard_static_http_server/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
I'm willing to bet most of you are familiar with HTTP, you're using it right now to read this content. If you've ever done any web programming you probably interacted with someone else's HTTP server stack - Flask, Apache, Nginx, Rack, etc. 

For today's challenge, the task is to implement your own HTTP server. No borrowing your language's built in server (e.g. no, you can't just use Python's SimpleHTTPServer). The rules, requirements, and constraints:


```
socket()
```

```
socket() ... bind() ... listen() ... accept() ...
```
A basic, bare bones HTTP/1.0 request looks like this;

(https://www.w3.org/Protocols/HTTP/1.0/spec.html#Request)

```
GET /index.html HTTP/1.0
```
That's it, no Host header required etc., and all other headers like user-agent and such are optional. (HTTP/1.1 requires a host header, in contrast.)

A basic, bare bones HTTP/1.0 response looks like this:

(https://www.w3.org/Protocols/HTTP/1.0/spec.html#Response)

```
HTTP/1.0 200 OK
Content-type: text/html

<H1>Success!</H1>
```
The first line indicates the protocol (HTTP/1.0), the resulting status code (200 in this case means "you got it"), and the text of the status. The next line sets the content type for the browser to know how to display the content. Then a blank line, then the actual content. Date, server, etc headers are all optional. 

Here's some basics on HTTP/1.0: http://tecfa.unige.ch/moo/book2/node93.html

(http://tecfa.unige.ch/moo/book2/node93.html)
Once you have this in your stash, you'll not only understand what more fully-featured servers like Apache or Nginx are doing, you'll have one you can customize. For example, I'm looking at extending my solution in C with an embedded Lua interpreter. 

(https://www.lua.org/)
# Bonus
Support threading for multiple connections at once. 

Support HEAD requests.

(https://www.w3.org/Protocols/HTTP/1.0/spec.html#HEAD)
Support POST requests. 

(https://www.w3.org/Protocols/HTTP/1.0/spec.html#POST)

----
## **DISCLAIMER**
This prompt has been adapted from [322 [Hard] Static HTTP Server](https://www.reddit.com/r/dailyprogrammer/comments/6lti17/20170707_challenge_322_hard_static_http_server/
)
