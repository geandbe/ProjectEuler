// Project Euler Problem 16
// http://projecteuler.net/problem=16
module ProjectEuler.Problem016

let inline digit (x: char) = int(x) - int('0')

let aggregate (ss: seq<_>) =
    1I <<< 1000
    |> sprintf "%A"
    |> fun x -> x.ToCharArray()
    |> Array.map digit
    |> Array.sum

//let problem016 () =
//    Seq.empty
//    |> solve PassAll PassAll aggregate

let problem016 () =
    1I <<< 1000
    |> sprintf "%A"
    |> fun x -> x.ToCharArray()
    |> Array.map digit
    |> Array.sum
