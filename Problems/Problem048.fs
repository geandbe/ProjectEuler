// Project Euler Problem 48
// http://projecteuler.net/problem=48
module ProjectEuler.Problem048

open System.Numerics

let lastTen = 10000000000I

let problem048 () =
    [1I..1000I]
    |> Seq.fold (fun acc i -> acc + bigint.ModPow(i,i,lastTen)) 0I
    |> fun x -> bigint.ModPow(x,1I,lastTen)