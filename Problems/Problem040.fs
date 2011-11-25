// Project Euler Problem 40
// http://projecteuler.net/problem=40
module ProjectEuler.Problem040

open System

let concatSeq len ss =
    new string (ss
        |> Seq.collect (fun s -> Seq.ofArray <| s.ToString().ToCharArray())
        |> Seq.take len
        |> Seq.toArray)

let digits = Seq.initInfinite (fun n -> n) |> concatSeq 1000001

let problem040 () =
    [1;10;100;1000;10000;100000;1000000]
    |> List.fold (fun result x -> result * Int32.Parse(string digits.[x])) 1
