// Project Euler Problem 19
// http://projecteuler.net/problem=19
module ProjectEuler.Problem019

open System

let problem019 () =
    Seq.unfold (fun x ->
                    if x > DateTime (2000,12,31) then None
                    else Some (x, x.AddMonths(1)))
               (DateTime(1901,1,1))
    |> solve PassAll (fun x -> x.DayOfWeek = DayOfWeek.Sunday) Seq.length