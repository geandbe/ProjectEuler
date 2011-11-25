// Project Euler Problem 37
// http://projecteuler.net/problem=37
module ProjectEuler.Problem037

open System

#nowarn "40"

let rec primes = 
    Seq.cache <| seq { yield 2; yield! Seq.unfold nextPrime 3 }
and nextPrime n =
    if isPrime n then Some(n, n + 2) else nextPrime(n + 2)
and isPrime n =
    if n >= 2 then
        primes 
        |> Seq.tryFind (fun x -> n % x = 0 || x * x > n)
        |> fun x -> x.Value * x.Value > n
    else false

let isTruncatableL n =
    [let ns = string n
    for i = 0 to (String.length ns) - 1 do
        yield Convert.ToInt32(ns.Substring(i))] |> List.forall isPrime

let isTruncatableR n =
    [let ns = string n
    for i = (String.length ns) downto 1 do
        yield Convert.ToInt32(ns.Substring(0,i))] |> List.forall isPrime
        
let isTruncatable n = n > 7 && isTruncatableL n && isTruncatableR n

let problem037 () =
    primes
    |> solve PassAll isTruncatable (Seq.take 11 >> Seq.sum)