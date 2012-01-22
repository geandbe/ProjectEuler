// Project Euler Problem 24
// http://projecteuler.net/problem=24
module ProjectEuler.Problem024

open System

let fact n = [1..n] |> Seq.reduce (*)

let exclude elem = set >> Set.remove elem >> Set.toArray

let lexPermutation nth (digits: int []) =
    if nth < 1 || nth > fact (digits.Length) then
        failwith "Permutation # is out of valid range"

    let rec permutator acc permutationsLeft (members: int []) =
        if members.Length = 1 then
            members.[0] :: acc |> List.rev |> List.toArray 
        else
            let divider = fact (members.Length) / members.Length
            let idx = permutationsLeft  / divider
            let next = members.[idx]
            permutator
                (next::acc) (permutationsLeft - divider*idx) (exclude next members)
    
    permutator [] (nth - 1) digits // first permutation is 0th, indeed

//let problem024 () =
//    Seq.empty
//    |> solve PassAll PassAll (fun _ ->
//                                lexPermutation 1000000 [|0..9|]
//                                |> Array.map string |> fun x -> String.Join("", x))

let problem024 () =
    lexPermutation 1000000 [|0..9|]
    |> Array.map string |> fun x -> String.Join("", x)