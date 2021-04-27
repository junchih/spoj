module Prime1

let rec rangeCheck seed curr limit =
    let primeCheck seed num =
        Array.forall (fun p -> num % p <> 0) seed

    let primeAppend seed num =
        if primeCheck seed num then
            Array.append seed [| num |]
        else
            seed

    if curr >= limit then
        seed
    else
        rangeCheck (primeAppend seed curr) (curr + 1) limit

let primes0 min max =

    let mid = sqrt (double max) |> int
    let mid = List.min [ mid; (min - 1) ]

    let seed = rangeCheck [||] 2 (mid + 1)
    let seedLen = seed.Length

    let primes = rangeCheck seed min (max + 1)
    Array.sub primes seedLen (primes.Length - seedLen)

let primes min max =
    let min = List.max [ 2; min ]

    if min > max then
        [||]
    else
        primes0 min max

open System

let main (argv: string array) =
    let line = Console.ReadLine()
    let count = int line

    for ignore = 1 to count do
        let line = Console.ReadLine().Split [| ' ' |]
        let min = int line.[0]
        let max = int line.[1]
        let min = List.max [ min; 2 ]
        let primes = primes min max

        for prime in primes do
            printf "%d\n" prime

        printf "\n"

    0

[||] |> main |> exit

(*
package main

import (
	"fmt"
	"math"
)

func main() {

	var t int
	fmt.Scanln(&t)

	for ; t > 0; t-- {

		var m, n int32
		fmt.Scanln(&m, &n)

		k := math.Min(math.Sqrt(float64(n)), float64(m-1))
		primes := Primes(nil, 2, int32(k)+1)
		mi := len(primes)
		if m < 2 {
			primes = Primes(primes, 2, n+1)
		} else {
			primes = Primes(primes, m, n+1)
		}

		for i := mi; i < len(primes); i++ {
			fmt.Println(primes[i])
		}

		fmt.Println()
	}
}

func Primes(primes []int32, lower, upper int32) []int32 {

	for num := lower; num < upper; num++ {

		isPrime := true
		for i := 0; i < len(primes); i++ {
			if num%primes[i] == 0 {
				isPrime = false
				break
			}
		}

		if isPrime {
			primes = append(primes, num)
		}
	}

	return primes
}
*)
