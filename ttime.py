import threading
import time
import sys

def main():
  threads = []
  for i in range(10):
    t = threading.Thread(target=work)
    t.start()
    threads.append(t)
  for t in threads:
    t.join()

def work():
  start = time.time()
  time.sleep(2)
  end = time.time()
  print 'took: %.3f\n' % (end - start)

if __name__ == "__main__":
  main()
