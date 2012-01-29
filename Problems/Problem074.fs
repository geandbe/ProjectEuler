// Project Euler Problem 74
// http://projecteuler.net/problem=74

module ProjectEuler.Problem074

open System.Collections.Generic

let digitFact  = function
    | 0 | 1 -> 1 | 2 -> 2 | 3 -> 6 | 4 -> 24 | 5 -> 120
    | 6 -> 720 | 7 -> 5040 | 8 -> 40320 | 9 -> 362880
    | _ -> failwith "bad argument"

let asSumOfDigitsFact n =
    Seq.unfold (fun x ->
        if x = 0 then None else Some(digitFact (x%10), x/10) ) n
    |> Seq.sum

let cache = new Dictionary<int,int>()
cache.Add(169,3); cache.Add(363601,3); cache.Add(1454,3)
cache.Add(871,2); cache.Add(872,2); cache.Add(45361,2); cache.Add(45362,2)

let chainLen n =
    let rec buildChain prev len =
        match asSumOfDigitsFact prev with
        | x when x = prev -> len
        | _ as x -> match cache.TryGetValue x with
                        | true, l -> len + l
                        | false, _ -> buildChain x len + 1

    match cache.TryGetValue n with
    | true, x -> x
    | false, _ ->
        let len = buildChain n 1
        cache.Add(n, len)
        len

let problem074() =
    [1..999999]
    |> List.map chainLen
    |> List.filter (fun x -> x = 60)
    |> List.length