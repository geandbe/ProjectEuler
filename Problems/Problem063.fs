// Project Euler Problem 63
// http://projecteuler.net/problem=63
module ProjectEuler.Problem063

open System

let nthPowersAmount ``base`` =
    let nDigits n = n.ToString().Length

    let power n (``base``: bigint) =
        [1..n] |> List.fold (fun product _ -> ``base`` * product) 1I

    Seq.unfold (fun n ->
                    if (power n ``base``) |> nDigits <> n then None
                    else Some(1, n + 1)) 1
    |> Seq.length

let problem063 () =
    {1I..9I} |> Seq.map nthPowersAmount |> Seq.sum