// Project Euler Problem 14
// http://projecteuler.net/problem=14
module ProjectEuler.Problem014

let numTerms (n: int64) = 
    Seq.unfold (fun x ->
        if x = 1L then None
        elif x%2L = 0L then Some(x, x/2L)
        else Some(x, 3L*x + 1L)) n
    |> Seq.length |> fun x -> (n, x+1)

let problem014 () =
    [1L..999999L] |> Seq.map numTerms
    |> solve PassAll PassAll (Seq.maxBy snd >> fst)

