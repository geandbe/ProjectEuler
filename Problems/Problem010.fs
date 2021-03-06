﻿// Project Euler Problem 10
// http://projecteuler.net/problem=10
module ProjectEuler.Problem010

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

//let problem010 () =
//    primes
//    |> solve ((>) 2000000) PassAll (Seq.map int64 >> Seq.sum)

let problem010 () =
    primes
    |> Seq.takeWhile ((>) 2000000)
    |> (Seq.map int64 >> Seq.sum)
// Leftovers from performance measurements
//    let time f () =
//        let t = System.Diagnostics.Stopwatch()
//        t.Start()
//        printfn "Fired"
//        try
//            f ()
//        finally
//            printfn "Took %dms\n" t.ElapsedMilliseconds
//
//    time doit ()
