module Dotaa

    let tryPassHeros towers damage heros =
        let passCounts = List.map (fun hero -> (hero - 1) / damage) heros
        (List.sum passCounts) >= towers

    let toYN b =
        if b then
            "YES"
        else
            "NO"

    open System

    let main (argv: string array) =
        let count = Console.ReadLine() |> int
        for ignore = 1 to count do
            let line = Console.ReadLine().Split [|' '|]
            let heroCount = int line.[0]
            let towers = int line.[1]
            let damage = int line.[2]
            let heros = [for ignore = 1 to heroCount do
                            yield Console.ReadLine() |> int]
            let ok = tryPassHeros towers damage heros |> toYN
            printf "%s\n" ok
        0

    [||] |> main |> exit

(*
module Dotaa where

pass_Hs m _D [] = m <= 0 || _D <= 0
pass_Hs m _D (_H:_Hs) =
  if m <= 0
    then True
    else pass_Hs (m - ((_H - 1) `div` _D)) _D _Hs

main = interact f
  where
    f str =
      let t:nums = map read (words str)
          test 0 nums = []
          test t (n:m:_D:nums) = stat : (test (t - 1) (drop n nums))
            where
              stat = pass_Hs m _D (take n nums)
          to_YN True = "YES"
          to_YN False = "NO"
          yns = map to_YN (test t nums)
      in unlines yns
*)

(*
package main

import (
	"fmt"
)

/*
 *  this implementation has LTS. see the alternative in haskell dotaa.hs
 */
func main() {

	var t int
	fmt.Scanln(&t)

	for ; t > 0; t-- {

		var n, m, D, H int
		fmt.Scanln(&n, &m, &D)

		passed := 0
		heroi := 0

		for ; heroi < n && passed < m; heroi++ {
			fmt.Scanln(&H)
			passed += (H - 1) / D
		}

		for ; heroi < n; heroi++ {
			var dummy string
			fmt.Scanln(&dummy)
		}

		if passed >= m {
			fmt.Println("YES")
		} else {
			fmt.Println("NO")
		}
	}
}
*)
