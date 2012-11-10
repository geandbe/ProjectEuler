// Project Euler Problem 89
// http://projecteuler.net/problem=89
module ProjectEuler.Problem089

open System.IO

let rec reduce saved (roman: string)  =
    match ([| "IIII"; "VIIII"; "XXXX"; "LXXXX"; "CCCC"; "DCCCC" |]
        |> Array.map (fun x -> (x, roman.IndexOf(x)))
        |> Array.filter (fun (x, i) -> i >= 0)) with
    | [||] -> saved
    | _ as a -> a |> Array.minBy(fun (x,i) -> i)
                  |> fun (x,i) -> reduce (saved + x.Length - 2) (roman.Substring(i + x.Length))

let problem089() =
    File.ReadAllText( @"..\..\..\Datafiles\Problem089.data") |> reduce 0