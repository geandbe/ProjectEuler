// Project Euler Problem 25
// http://projecteuler.net/problem=25
module ProjectEuler.Problem025

let inline nDigits n = String.length <| (n.ToString())

let fibnums =
    Seq.unfold (fun (current, next) ->
        Some(current, (next, current+next)))(0I,1I)

//let problem025 () =
//    fibnums
//    |> Seq.takeWhile (fun x -> nDigits x < 1000)
//    |> solve PassAll PassAll Seq.length

let problem025 () =
    fibnums
    |> Seq.takeWhile (fun x -> nDigits x < 1000)
    |> Seq.length