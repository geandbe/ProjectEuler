// Project Euler Problem 74
// http://projecteuler.net/problem=74

module ProjectEuler.Problem074

let digitFact  = function
    | 0 | 1 -> 1 | 2 -> 2 | 3 -> 6 | 4 -> 24 | 5 -> 120
    | 6 -> 720 | 7 -> 5040 | 8 -> 40320 | 9 -> 362880
    | _ -> failwith "bad argument"

let asSumOfDigitsFact n =
    Seq.unfold (fun x ->
        if x = 0 then None else Some(digitFact (x%10), x/10) ) n
    |> Seq.sum

let cache = Array.zeroCreate ((asSumOfDigitsFact 999999) + 1)
cache.[169] <- 3; cache.[363601] <- 3; cache.[1454] <- 3
cache.[871] <- 2; cache.[872] <- 2;
cache.[45361] <- 2; cache.[45362] <- 2; 

let chainLen n =
    let rec buildChain prev len =
        match asSumOfDigitsFact prev with
        | x when x = prev -> len
        | _ as x -> match cache.[x] with
                        | 0 -> buildChain x len + 1
                        | _ as l -> len + l

    match cache.[n] with
    | 0 ->
        let len = buildChain n 1
        cache.[n] <- len
        len
    | _ as x -> x

let problem074() =
    [1..999999]
    |> List.map chainLen
    |> List.filter (fun x -> x = 60)
    |> List.length