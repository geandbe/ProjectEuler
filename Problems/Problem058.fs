// Project Euler Problem 58
// http://projecteuler.net/problem=58
module ProjectEuler.Problem058


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

let problem058() =
    let morePrimes side last =
        seq {for corner in [1..4] -> last + corner * (side - 1)}
        |> Seq.filter isPrime |> Seq.length

    Seq.initInfinite (fun i -> 2*i + 1) |> Seq.skip 1
    |> Seq.scan (fun (prevSide, last, all, primeMembers) side ->
                    (side, last + 4*(side - 1), all + 4, primeMembers + (morePrimes side last))) (1,1,1,0)
    |> Seq.find (fun (_, _, all, primeMembers) -> all > 1 && primeMembers * 10 <= all)
    |> fun (side, _, _, _) -> side