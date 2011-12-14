// Project Euler Problem 60
// http://projecteuler.net/problem=60
module ProjectEuler.Problem060

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

let concatenate n1 n2 =
    Int32.Parse(n1.ToString() + n2.ToString())

let rec canAdd xs x =
    match xs with
    | [] -> true
    | h :: t -> if isPrime (concatenate x h) && isPrime (concatenate h x) then
                    canAdd t x
                else false

let subSequence lower upper =
    Seq.skipWhile ((>=) lower) >> Seq.takeWhile ((>) upper)

let seed lower upper =
    primes
    |> subSequence lower upper
    |> Seq.map (fun x -> [x])
    |> Seq.toList

let nextLevelItem ls upper =
    primes |> subSequence (List.head ls) upper
    |> Seq.zip (Seq.initInfinite (fun x -> ls))
    |> Seq.filter (fun (xs,x) -> canAdd xs x)
    |> Seq.map (fun (xs,x) -> x :: xs) |> Seq.toList


let rec nextLevel ps upper =
    match ps with
    | [] -> []
    | h :: t -> List.append (nextLevelItem h upper) (nextLevel t upper)

let problem060 () =
    nextLevel (nextLevel (nextLevel (nextLevel (seed 7 19) 10000) 10000) 10000) 10000
    |> List.map List.sum
    |> List.min
