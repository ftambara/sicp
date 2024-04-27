package main

import "time"

// timeFunc measures the time it takes to run a function in milliseconds.
func timeFunc(f func()) float64 {
	start := time.Now()
	f()
	return float64(time.Since(start).Milliseconds())
}

func isPrime(n int) bool {
	if n < 2 {
		return false
	}
	if n%2 == 0 {
		return false
	}
	for i := 3; i*i <= n; i += 2 {
		if n%i == 0 {
			return false
		}
	}
	return true
}

func sumOfPrimes(start, end int) int {
	sum := 0
	for i := start; i < end; i++ {
		if isPrime(i) {
			sum += i
		}
	}
	return sum
}

func nthPrime(start, end, n int) (nthPrime int, found bool) {
	for i := start; i < end; i++ {
		if isPrime(i) {
			n--
			if n == 0 {
				return i, true
			}
		}
	}
	return 0, false
}

func main() {
	println(timeFunc(func() {
		println(sumOfPrimes(1, 1_000_000))
	}))

	println(timeFunc(func() {
		println(nthPrime(1_000_000, 10_000_000, 2))
	}))
}
