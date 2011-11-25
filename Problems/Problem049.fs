// Project Euler Problem 49
// http://projecteuler.net/problem=49
module ProjectEuler.Problem049


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

let arePermutes n =
    let toSet n = n.ToString().ToCharArray() |> set
    (toSet (n + 3330)).Equals((toSet n)) && (toSet (n + 6660)).Equals((toSet n))

let isUnusual n =
    (isPrime (n + 3330)) && (isPrime (n + 6660)) && (arePermutes n)

let problem049 () =
    primes
    |> Seq.skipWhile ((>=) 1489)
    |> Seq.find isUnusual
    |> fun x -> sprintf "%d%d%d" x (x+3330) (x+6660)
