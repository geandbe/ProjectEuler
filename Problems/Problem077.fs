// Project Euler Problem 77
// http://projecteuler.net/problem=77

module ProjectEuler.Problem077

// Prime generator from Problem 10
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

// Partition from prime parts
let parts target =
    primes |> Seq.takeWhile ((>=) (target - 2)) |> Seq.toArray

// Partitioning counter from Problem 31
let partitions target (parts: int[]) =
    let ways = Array.zeroCreate (target + 1)
    ways.[0] <- 1
    for i in 0..(parts.Length - 1) do
        for j in parts.[i]..target do
            ways.[j] <- ways.[j] + ways.[j - parts.[i]]
    ways.[target]

let problem077 () =
    Seq.initInfinite id
    |> Seq.find (fun x -> partitions x (parts x) > 5000)

