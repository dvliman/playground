package main

import (
  "fmt"
  "time"
  "runtime"
)

const maxThreads int = 10

func main() {
  var numThreads int 
  numThreads = 400000
  fmt.Printf("Number of threads: %d\n", numThreads)

  numCPU := runtime.NumCPU()
  fmt.Printf("Number of CPU: %d\n", numCPU)

  runtime.GOMAXPROCS(numCPU)

  var ch chan int
  ch = make(chan int)

  t1 := time.Now()
  for i := 0; i <numThreads; i++ {
    go wait_4_cmd(i, ch)
  }

  for i := 0; i <numThreads; i++ { 
    ch <- -1
  }

  d := time.Since(t1)

  fmt.Printf("Total time: %s\n", d.String())
  fmt.Printf("Average   : %f microseconds\n", float64(d) / float64(numThreads) / 1000.0)
}


func wait_4_cmd(id int, ch chan int) {
    for {
    cmd := <- ch
    if cmd == -1 {
      return
    }
  }

}
