// Project Euler Problem 22
// http://projecteuler.net/problem=22
module ProjectEuler.Problem022

open System.IO

let alphavalue (s: string) =
    s.ToCharArray() |> Array.map (fun x -> int x - int 'A' + 1) |> Array.sum

let readData (path: string) =
    use sr = new StreamReader(path)  
    sr.ReadLine().Split(',')

//let problem022 () =
//    readData @"..\..\..\Datafiles\Problem022.data"
//    |> Array.map (fun x -> x.Replace("\"",""))
//    |> Array.sort
//    |> Array.mapi (fun i x -> int64((alphavalue x) * (i + 1)))
//    |> Array.toSeq
//    |> solve PassAll PassAll Seq.sum

let problem022 () =
    readData @"..\..\..\Datafiles\Problem022.data"
    |> Array.map (fun x -> x.Replace("\"",""))
    |> Array.sort
    |> Array.mapi (fun i x -> int64((alphavalue x) * (i + 1)))
    |> Array.sum