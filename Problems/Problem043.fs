// Project Euler Problem 43
// http://projecteuler.net/problem=43
module ProjectEuler.Problem043

open System

//---J.Harrop F# for Scientists; pp.166-167---------
let rec distribute e = function
   | [] -> [[e]]
   | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)
//--------------------------------------------------

let isDivisible (l: int list) =
    let p = List.toArray l
    (100*p.[1]+10*p.[2]+p.[3])%2=0 &&
    (100*p.[2]+10*p.[3]+p.[4])%3=0 &&
    (100*p.[3]+10*p.[4]+p.[5])%5=0 &&
    (100*p.[4]+10*p.[5]+p.[6])%7=0 &&
    (100*p.[5]+10*p.[6]+p.[7])%11=0 &&
    (100*p.[6]+10*p.[7]+p.[8])%13=0 &&
    (100*p.[7]+10*p.[8]+p.[9])%17=0

let glueUp (l: int list) =
    l |> List.rev
    |> List.zip [1;10;100;1000;10000;100000;1000000;10000000;100000000;1000000000]
    |> List.map (fun (x,y) -> (int64 x) * (int64 y))
    |> List.sum

let problem043 () =
    permute [0;1;2;3;4;5;6;7;8;9]
    |> List.filter isDivisible
    |> List.map glueUp
    |> List.sum
