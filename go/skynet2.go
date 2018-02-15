package main

import "fmt"
import "time"

func skynet(num int, size int, div int) int {
	if size == 1 {
		return num
	}
	var sum int = 0
	for i := 0; i < div; i++ {
		subNum := num + i*(size/div)
		sum += skynet(subNum, size/div, div)
	}
	return sum
}

func main() {
	start := time.Now()
	result := skynet(0, 1000000, 10)
	took := time.Since(start)
	fmt.Printf("Result: %d in %d ms.\n", result, took.Nanoseconds()/1e6)
}
