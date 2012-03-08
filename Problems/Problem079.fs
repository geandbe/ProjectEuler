// Project Euler Problem 79
// http://projecteuler.net/problem=79

module ProjectEuler.Problem079

open System.IO
open System

let getData (path: string) =
    [ use sr = new StreamReader(path)
      while not sr.EndOfStream do
      yield sr.ReadLine() |> int ]

let pairs =
    getData @"..\..\..\Datafiles\Problem079.data"
    |> set |> Set.toSeq
    |> Seq.collect (fun x ->
                    x.ToString().ToCharArray()
                    |> fun y -> seq {
                                        yield (y.[0], y.[1])
                                        yield (y.[1], y.[2])
                                    })
    |> set |> Set.toList


let last ts =
    ts |> List.fold (fun (f,t) (x,y) ->
        ((Set.add x f), (Set.add y t))) (Set.empty,Set.empty)
    |> fun (x,y) -> Set.difference y x |> Set.toList |> List.head

let cut i ts = ts |> List.fold (fun ts' (x,y) ->
    if y <> i then (x,y) :: ts' else ts') []

let rec pull pairs chain =
    match pairs with
    | [(x,y)] -> x::y::chain
    | _ -> pull (cut (last pairs) pairs) ((last pairs)::chain)

let problem079 () =
    pull pairs [] |> List.toArray |> fun x -> Int32.Parse(new string(x))