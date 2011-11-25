// Project Euler Problem 47
// http://projecteuler.net/problem=47
module ProjectEuler.Problem047

let primeFactors n =
    let rec factorize n f =
        seq {
        if n <= f then
            yield f
        else
            match n%f with
            | 0 -> yield f; yield! factorize (n/f) f
            | _ -> yield! factorize n (f + 1)
        }

    factorize n 2

let fourPrimes (a: int array) =
    primeFactors a.[0] |> set |> Set.count = 4 &&
    primeFactors a.[1] |> set |> Set.count = 4 &&
    primeFactors a.[2] |> set |> Set.count = 4 &&
    primeFactors a.[3] |> set |> Set.count = 4

let problem047 () =
    Seq.initInfinite id
    |> Seq.windowed 4
    |> Seq.find fourPrimes
    |> Array.get <| 0
