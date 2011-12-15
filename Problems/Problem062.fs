// Project Euler Problem 62
// http://projecteuler.net/problem=62
module ProjectEuler.Problem062

open System
open System.Collections.Generic

let hash n =
    n.ToString().ToCharArray() |> Array.sort |> fun x -> new String(x)

let permutations = new Dictionary<String, int*int64>()

let is5thCube permutated =
    let key = hash permutated
    if permutations.ContainsKey(key) then
        let hasOccured, cube = permutations.Item key
        match hasOccured with
        | 4 -> true
        | _ -> permutations.Item key <- (hasOccured + 1, cube)
               false
    else
        permutations.Add (key, (1, permutated))
        false

let problem062 () =
    Seq.initInfinite (fun x -> (int64 x) * (int64 x) * (int64 x))
    |> Seq.find is5thCube
    |> fun permutated -> snd (permutations.Item (hash permutated))