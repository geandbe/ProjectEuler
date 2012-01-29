// Project Euler Problem 74
// http://projecteuler.net/problem=74

module ProjectEuler.Problem074

open System.Collections.Generic

let digitFact  = function
    | 0 | 1 -> 1
    | 2 -> 2
    | 3 -> 6
    | 4 -> 24
    | 5 -> 120
    | 6 -> 720
    | 7 -> 5040
    | 8 -> 40320
    | 9 -> 362880
    | _ -> failwith "bad argument"

let asSumOfDigitsFact n =
    Seq.unfold (fun x ->
        if x = 0 then None else Some(digitFact (x%10), x/10) ) n
    |> Seq.sum

//let chainLen n =
//    let rec buildChain prev len =
//        match asSumOfDigitsFact(prev) with
//        | 169 | 363601 | 1454 -> if len = 1 then 3 else len + 3
//        | 871 | 872 | 45361 | 45362 -> if len = 1 then 2 else len + 2
//        | _ as x -> if x = prev then len else buildChain x (len+1)
//    buildChain n 1
let dict = new Dictionary<int,int>()

let chainLen n =
    let rec buildChain prev len =
        match asSumOfDigitsFact(prev) with
        | 169 | 363601 | 1454 -> if len = 1 then 3 else len + 3
        | 871 | 872 | 45361 | 45362 -> if len = 1 then 2 else len + 2
        | _ as x -> if x = prev then len else buildChain x (len+1)

    match dict.TryGetValue(n) with
    | true, x -> x
    | false, _ ->
        let len = buildChain n 1
        dict.Add(n, len)
        len

let problem074() =
    [1..999999] |> List.map chainLen |> List.filter (fun x -> x = 60) |> List.length