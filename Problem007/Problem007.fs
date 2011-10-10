// Project Euler Problem 7
// http://projecteuler.net/problem=7
module ProjectEuler

// Descending order of already found primes in the list
let primesD =
    let rec findNextPrime maybePrime lesserPrimes =
        match List.tryFind(fun x -> maybePrime % x = 0) lesserPrimes with
        | Some _ -> findNextPrime (maybePrime + 2) lesserPrimes
        | None -> maybePrime

    Seq.append {2..3} (Seq.unfold (fun (lastPrime, lessOrEqPrimes) ->
                    let nextPrime = findNextPrime (lastPrime + 2) lessOrEqPrimes
                    Some (nextPrime, (nextPrime, nextPrime::lessOrEqPrimes))) (3,[3]))

// Ascending order of already found primes in the list
let primesA =
    let rec findNextPrime maybePrime lesserPrimes =
        match List.tryFind(fun x -> maybePrime % x = 0) lesserPrimes with
        | Some _ -> findNextPrime (maybePrime + 2) lesserPrimes
        | None -> maybePrime

    Seq.append {2..3}
        (Seq.unfold (fun (lastPrime, lessOrEqPrimes) ->
            let nextPrime = findNextPrime (lastPrime + 2) lessOrEqPrimes
            Some (nextPrime, (nextPrime, lessOrEqPrimes @ [nextPrime])))
                (3,[3]))

let problem007 () =
    primesA
    |> solve PassAll PassAll (Seq.skip 10000 >> Seq.head)
