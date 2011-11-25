// Project Euler Problem 27
// http://projecteuler.net/problem=27
module ProjectEuler.Problem027

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
    
let inline form a b n = n * n + a * n + b

let mutable max = 0
let mutable result = 0

let problem027 () =
    for a = -999 to 999 do
        for b = -999 to 999 do
            Seq.unfold (fun n -> if isPrime (form a b n) then Some(n, n+1) else None) 0
            |> Seq.length |> fun x -> if x > max then max <- x; result <- a * b
    result
