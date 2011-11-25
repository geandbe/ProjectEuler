// Project Euler Problem 46
// http://projecteuler.net/problem=46
module ProjectEuler.Problem046

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

let oddComposites =
    Seq.unfold (fun x -> Some(x, x+2)) 3 |> Seq.filter (isPrime >> not)

let squares =
    Seq.cache <| seq { yield 1;
                       yield! Seq.unfold (fun n -> Some(n*n + 2*n + 1, n+1)) 1 }
    
let falseConjecture n =
    let someSquares = primes |> Seq.skip 1 |> Seq.takeWhile ((>) n)
                      |> Seq.map (fun x -> (n - x) / 2) |> set
    let allSquares = squares
                     |> Seq.takeWhile ((>) (Set.maxElement someSquares)) |> set
    Set.intersect someSquares allSquares = Set.empty
 
let problem046 () =
    oddComposites |> Seq.find falseConjecture