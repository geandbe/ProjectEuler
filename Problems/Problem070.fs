// Project Euler Problem 70
// http://projecteuler.net/problem=70
module ProjectEuler.Problem070

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

let arePermutes n m =
    let inline canonic n =
        new string (Array.sort ((string n).ToCharArray()))
    canonic n = canonic m

let allPairs l1 l2 =
    List.map (fun x -> (List.map (fun y -> (x,y)) l2)) l1 |> List.concat

let problem070() =
    let meanFactor = 10000000 |> (float >> sqrt >> int)
    let factors = primes |> Seq.skipWhile ((>) (meanFactor - 1000))
                  |> Seq.takeWhile ((>) (meanFactor + 1000)) |> Seq.toList
    allPairs factors factors
    |> List.filter (fun (x,y) -> x*y < 10000000 &&
                                    arePermutes (x*y) ((x-1)*(y-1)))
    |> List.minBy (fun (x,y) -> float (x*y) / float ((x-1)*(y-1)))
    |> fun (x,y) -> x*y