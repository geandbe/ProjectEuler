// Project Euler Problem 20
// http://projecteuler.net/problem=20
module ProjectEuler.Problem020

let factorial (n: bigint) : bigint =
    [1I..n] |> List.reduce (*)

let problem020 () =
    factorial 100I
    |> fun x -> x.ToString().ToCharArray()
    |> Array.map (fun x -> (int x) - (int '0'))
    |> Array.toSeq
    |> solve PassAll PassAll Seq.sum