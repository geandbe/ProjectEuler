// Project Euler Problem 41
// http://projecteuler.net/problem=41
module ProjectEuler.Problem041

// Observations:
// sum(1,2,3...,9) = 45 => 9-digit pandigital cannot be prime
// sum(1,2,3,...,8) = 36 => 8-digit pandigital cannot be prime
// Hense, the sought number is less or equal to 7654321.
// Assume there is at least one 7-digit pandigital prime and brute force the proof

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

let set7 = ['1';'2';'3';'4';'5';'6';'7'] |> set

let isPandigital7 n =
    n.ToString().ToCharArray() |> Set.ofArray = set7

let problem041 () =
    primes
    |> solve ((>=) 7654321) PassAll
       (Seq.toList >> List.rev >> List.find (isPandigital7))
