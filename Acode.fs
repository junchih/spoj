module Acode
open System

let countAmbigs0 code (hdr, (si1, si2)) =
    let sis =
        if hdr < 0 then
            (1, 0)
        else if hdr = 0 then
            (0, si1 + si2)
        else if code = 0 || code * 10 + hdr > 26 then
            (si1 + si2, 0)
        else
            (si1 + si2, si1)
    (code, sis)

let countAmbigs ls =
    let (_, (s1, s2)) = List.foldBack countAmbigs0 ls (-1, (0, 0))
    s1 + s2

let main (argv: string array) =

    let rec proc (line: string) =

        let ls = Array.foldBack (fun ch ls -> (int ch - int '0')::ls) (line.ToCharArray()) []
        Console.WriteLine(countAmbigs ls)

        let next = Console.ReadLine()
        if next.[0] <> '0' then
            proc next

    let line = Console.ReadLine()
    if line.[0] <> '0' then
        proc line
    0

[||] |> main |> exit
