package main

import "fmt"
import "time"

func main() {
	go fmt.Println("Hello from another goroutine")
	fmt.Println("Hello from main goroutine")
	
	time.Sleep(time.Second)
}
