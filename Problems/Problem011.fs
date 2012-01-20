// Project Euler Problem 11
// http://projecteuler.net/problem=11

module ProjectEuler.Problem011

open System
open System.IO

type Direction = E | SE | S | SW
let span = 4
let side = 20

let grid: int[,] = Array2D.zeroCreate side side

let tracer =
    dict [
        (E,[(0,0); (1,0); (2,0); (3,0)]);
        (SE,[(0,0); (1,1); (2,2); (3,3)]);
        (S,[(0,0); (0,1); (0,2); (0,3)]);
        (SW,[(0,0); (-1,1); (-2,2); (-3,3)]);
    ]

let foldDirection direction x y =
    tracer.Item(direction)
    |> List.fold (fun acc (dx,dy) -> acc * grid.[x + dx, y + dy]) 1

let run direction x y =
    match direction with
    | E when x <= side - span -> foldDirection E x y
    | SE when x <= side - span && y <= side - span -> foldDirection SE x y
    | S when y <= side - span -> foldDirection S x y
    | SW when x >= span - 1 && y <= side - span -> foldDirection SW x y
    | _ -> Int32.MinValue

// Fill the grid from file
let fileContents = File.ReadAllLines(@"..\..\..\Datafiles\grid.data")
for x = 0 to side - 1 do
    let inputLine = fileContents.[x].Split(' ')
    for y = 0 to side - 1 do
        grid.[y,x] <- Convert.ToInt32(inputLine.[y])

let products =
    [
        for y in 0 .. side - 1 do
            for x in 0 .. side - 1 do
                yield run E x y; yield run SE x y;
                yield run S x y; yield run SW x y; 
    ]

//let problem011 () =
//    products
//    |> solve PassAll PassAll Seq.max

let problem011 () =
    products |> Seq.max