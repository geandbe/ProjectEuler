// Project Euler Problem 7
// http://projecteuler.net/problem=7
module ProjectEuler

let primes =
    let rec findNextPrime maybePrime lesserPrimes =
        match List.tryFind(fun x -> maybePrime % x = 0) lesserPrimes with
        | Some _ -> findNextPrime (maybePrime + 2) lesserPrimes
        | None -> maybePrime

    Seq.append {2..3} (Seq.unfold (fun (lastPrime, lessOrEqPrimes) ->
                    let nextPrime = findNextPrime (lastPrime + 2) lessOrEqPrimes
                    Some (nextPrime, (nextPrime, nextPrime::lessOrEqPrimes))) (3,[3;2]))

let problem007 () =
    primes
    |> solve PassAll PassAll (Seq.skip 10000 >> Seq.head)
