import threading
import time

from typing import List

links = [f"https://my.docs.com/{x}" for x in range(3)]

def crawl(link, delay=3):
    print(f"Crawl started for {link}")
    time.sleep(delay)
    print(f"Crawl ended for {link}")

threads: List[threading.Thread] = []
for link in links:
    # Represents an activity that is run in a separate thread of control.
    task = threading.Thread(target=crawl, args=(link,), kwargs={"delay": 2})
    threads.append(task)

for t in threads:
    t.start() # Starts the activity in a separate thread of control by calling `t.run()`.

for t in threads:
    t.join() # Blocks the current thread until `t` is terminated.

# Sample output
# Crawl started for https://my.docs.com/0
# Crawl started for https://my.docs.com/1
# Crawl started for https://my.docs.com/2
# Crawl ended for https://my.docs.com/0
# Crawl ended for https://my.docs.com/2
# Crawl ended for https://my.docs.com/1
