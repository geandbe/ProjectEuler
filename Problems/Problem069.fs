// Project Euler Problem 69
// http://projecteuler.net/problem=69
module ProjectEuler.Problem069

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

let problem069() =
    primes |> Seq.scan (fun acc x -> acc * x) 1
    |> Seq.takeWhile ((>) 1000000) |> Seq.max