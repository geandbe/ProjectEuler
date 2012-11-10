// Project Euler Problem 88
// http://projecteuler.net/problem=88
module ProjectEuler.Problem088

let K = 12000
let K2 = K*2

module List =
    let mul xs =
        xs |> List.reduce (fun a x -> a * x)

let rec moveNext = function
    | h::t as x when x.Length = 14 -> if (h + 1) * (List.mul t) <= K2 then
                                        (h + 1) :: t
                                      else moveNext t
    | [] -> []
    | h::t -> let mutable candidate = []
              for i in 1 .. (14 - t.Length) do
                  candidate <- (h + 1)::candidate
              candidate <- candidate @ t
              if (List.mul candidate) <= K2 then
                  candidate
              else moveNext t

let prodSums = [2;2;1;1;1;1;1;1;1;1;1;1;1;1]
               |> Seq.unfold (fun state ->
                  if state = [] then None
                  else Some(state, moveNext(state)))
               |> Seq.map (List.filter ((<>) 1))

let prodSumMins = Array.init (K + 1) ((*) 2)
prodSumMins.[1] <- 0

let factorsProductSum xs =
    let sum, prod = List.sum xs, List.mul xs
    (xs.Length + prod - sum, prod)

let updateMins = function
    size,prodSum -> if size <= K && prodSum < prodSumMins.[size]  then
                        prodSumMins.[size] <- prodSum

let problem088() =
    prodSums |> Seq.iter (factorsProductSum >> updateMins)
    prodSumMins |> set |> Set.toArray |> Array.sum

