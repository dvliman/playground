package main 

import (
  "fmt"
  "runtime" 
  "time"
)

func waitAround(die chan bool) {
  <- die 
}

func main() {
  cpus := runtime.NumCPU()
  runtime.GOMAXPROCS(cpus)

  count := 100000

  die := make(chan bool)

  var startMemory runtime.MemStats
  runtime.ReadMemStats(&startMemory)

  start := time.Now()
  for i := 0; i < count; i++ {
    go waitAround(die)
  }

  elapsed := time.Since(start)
  var endMemory runtime.MemStats
  runtime.ReadMemStats(&endMemory)

  fmt.Printf("Started %d goroutines on %d CPUs in %f seconds \n",
            count, cpus, elapsed.Seconds())

  fmt.Printf("Memory before %d, memory after %d \n", 
            startMemory.Alloc, endMemory.Alloc)

  fmt.Printf("%d goroutines running\n", runtime.NumGoroutine())

  close(die)
}

