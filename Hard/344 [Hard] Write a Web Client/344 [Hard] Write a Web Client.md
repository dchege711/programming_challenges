---
draft: true
----

# [344 (Hard) Write a Web Client](https://www.reddit.com/r/dailyprogrammer/comments/7jzy8k/20171215_challenge_344_hard_write_a_web_client/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
Today's challenge is simple: write a web client from scratch. Requirements:

For the challenge, your requirements are similar to the HTTP server challenge - implement a thing you use often from scratch instead of using your language's built in functionality:


```
urllib
```

```
httplib
```

```
requests
```

```
curl
```

```
curl
```

```
system("curl %s", url))
```

```
urlparse
```

```
java.net.URL
```
(http://server.io:8080/)

```
socket()
```
A good test server is httpbin, which can give you all sorts of feedback about your client's behavior; another is requestb.in.

(https://stackoverflow.com/questions/5725430/http-test-server-that-accepts-get-post-calls)
(https://requestb.in/)
# Example Output
Here is some simple bare-bones output from httpbin.org:


```
HTTP/1.1 200 OK
Connection: keep-alive
Server: meinheld/0.6.1
Date: Fri, 15 Dec 2017 17:14:03 GMT
Content-Type: application/json
Access-Control-Allow-Origin: *
Access-Control-Allow-Credentials: true
X-Powered-By: Flask
X-Processed-Time: 0.00114393234253
Content-Length: 158
Via: 1.1 vegur

{
  "args": {},
  "headers": {
    "Connection": "close",
    "Host": "httpbin.org"
  },
  "origin": "68.40.199.183",
  "url": "http://httpbin.org/get"
}
```
If your client can emit that kind of thing to standard out, you're set. 

# Bonus
The above focuses on a simple client. Here are a few more things you can do to extend it:


----
## **DISCLAIMER**
This prompt has been adapted from [344 [Hard] Write a Web Client](https://www.reddit.com/r/dailyprogrammer/comments/7jzy8k/20171215_challenge_344_hard_write_a_web_client/)
