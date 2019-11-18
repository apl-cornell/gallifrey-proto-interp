# just a file for some quick sanity checks

from threading import Thread
import time

t = Thread(target=(lambda: time.sleep(5) == print("B")))
t.start()
t.join()
print("A")

