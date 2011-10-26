// Project Euler Problem 18
// http://projecteuler.net/problem=18
module ProjectEuler.Problems018_067

open System.IO
open System

let getData (path: string) =
    [
        use sr = new StreamReader(path)
        while not sr.EndOfStream do
            yield (sr.ReadLine().Split(' ')
                  |> Array.map (fun x -> int x)
                  |> Array.toList)
    ]

let pair l =
    List.zip (0::l) (l @ [0])

let maxpath curr next =
    List.map2 (fun (x,y) z -> max (x+z) (y+z)) curr next

let rec findMaxPath l =
    match l with
    | first::second::t -> findMaxPath ((maxpath (pair first) second) :: t)
    | _ -> List.max (List.head l)

let problem018 () =
    Seq.empty |> solve PassAll PassAll (fun _ -> findMaxPath (getData @"..\..\..\Datafiles\Problem018.data"))

let problem067 () =
    Seq.empty |> solve PassAll PassAll (fun _ -> findMaxPath (getData @"..\..\..\Datafiles\Problem067.data"))
