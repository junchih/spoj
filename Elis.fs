module Elis

let longest longests limit =
    (0, 0)::longests
    |> List.filter (fun (num, siz) -> num < limit)
    |> List.maxBy (fun (num, siz) -> siz)

let longests ls =
    let ff longests num =
      let (_, len) = longest longests num
      (num, len+1)::longests
    List.fold ff [] ls

let longests' ls =
    longests ls
    |> List.maxBy (fun (_, siz) -> siz)
    |> fun (_, siz) -> siz

open System

let main (argv: string array) =
    let count = Console.ReadLine () |> int
    let line = Console.ReadLine ()
    let nums = Array.foldBack (fun num ls -> (int num)::ls) (line.Split " ") []
    Console.WriteLine (longests' nums)
    0

[||] |> main |> exit

(*
module Elis where

main = interact f
  where
    f str =
      let (_N:nums) = map read (words str)
          _A = take _N nums
          _L = elis _A
      in show _L

elis' [] = []
elis' (a:_A) =
  let longest _ [] [] = 0
      longest a (a':_A') (s':sums') =
        if a' > a
          then max s' (longest a _A' sums')
          else longest a _A' sums'
      sums = elis' _A
      curr = (longest a _A sums) + 1
  in curr : sums

elis _A = maximum (elis' _A)
*)
