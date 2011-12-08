// Project Euler Problem 59
// http://projecteuler.net/problem=59
module ProjectEuler.Problem059

open System
open System.IO

let encoded =
    use suckIn = new StreamReader @"..\..\..\Datafiles\Problem059.data"
    suckIn.ReadLine() |> fun x -> x.Split(',') |> Array.map Int32.Parse |> Array.map char

let textChars = [' '; '.'; '?'; '!'; ','; '"'; '\''; '('; ')'; ';']
                @ ['A'..'Z'] @ ['a'..'z'] @ ['0'..'9'] |> set
let inline isTextChar c = textChars.Contains c

let problem059 () =
    seq { for i in ['a'..'z'] do for j in ['a'..'z'] do  for k in ['a'..'z'] do yield seq [i; j; k] }
    |> Seq.map (fun x -> Seq.collect id (Seq.initInfinite (fun _ -> x)))
    |> Seq.map (Seq.zip encoded)
    |> Seq.map (Seq.map (fun (x,y) -> char ((int x) ^^^ (int y))))
    |> Seq.find (Seq.forall isTextChar) |> Seq.fold (fun s x -> s + (int x)) 0
