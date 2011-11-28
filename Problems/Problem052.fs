// Project Euler Problem 52
// http://projecteuler.net/problem=52
module ProjectEuler.Problem052

open System 

let wanted n =
    let multiples =
        [1;2;3;4;5;6] |> List.map (fun x -> (x * n).ToString())

    let multiplesHaveSameLength =
        (multiples |> List.map String.length |> set |> Set.count) = 1

    let multiplesHaveSameDigits =
        let digitSets = multiples
                        |> List.map (fun x -> x.ToCharArray() |> set)
                        |> List.toArray
        [for i in 0..4 -> digitSets.[i].Equals(digitSets.[i+1])]
        |> List.reduce (&&)
         
    multiplesHaveSameLength && multiplesHaveSameDigits

let problem052 () =
    Seq.initInfinite id
    |> Seq.skip 1
    |> Seq.find wanted