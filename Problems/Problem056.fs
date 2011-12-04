// Project Euler Problem 56
// http://projecteuler.net/problem=56
module ProjectEuler.Problem056

let sumOfDigits (n: bigint) =
    n.ToString().ToCharArray()
    |> Array.map (fun x -> (int)x - (int)'0') |> Array.sum

let powers ``base`` =
    Seq.unfold (fun (product, exponent) ->
        if (exponent < 100) then
            let interim = product * ``base``
            Some(interim, (interim, exponent + 1))
        else None) (1I,0)
    |> Seq.skip 90 |> Seq.toList

let problem056 () =
    [90I..99I] 
    |> List.collect powers
    |> List.map sumOfDigits
    |> List.max

// the same in Haskell :)
// maximum $map (\x -> foldl (\acc y -> acc + (digitToInt y)) 0 (show x)) [a^b | a <- [90..99], b <- [90..99]]