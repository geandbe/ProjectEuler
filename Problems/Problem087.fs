// Project Euler Problem 87
// http://projecteuler.net/problem=87
module ProjectEuler.Problem087

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

let TOPSUM = 50000000L
let TOPPRIME = int(sqrt (float TOPSUM))

let problem087() =
    let candidates = primes |> Seq.takeWhile (fun x -> x <= TOPPRIME) |> Seq.map int64
    let squares = candidates |> Seq.map (fun x -> x * x) |> Seq.toArray
    let cubes =
        candidates
        |> Seq.map (fun x -> x * x * x)
        |> Seq.takeWhile (fun x -> x < TOPSUM)
        |> Seq.toArray
    let fours =
        squares
        |> Seq.map (fun x -> x * x)
        |> Seq.takeWhile (fun x -> x < TOPSUM)
        |> Seq.toArray

    let unique = System.Collections.Generic.HashSet<int64>()
    for i in [0..squares.Length-1] do
        for j in [0..cubes.Length-1] do
            for k in [0..fours.Length-1] do
                let sum = squares.[i] + cubes.[j] + fours.[k] in
                    if sum <= TOPSUM then
                        unique.Add(sum) |> ignore
    unique.Count
