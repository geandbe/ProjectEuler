// Project Euler Problem 81
// http://projecteuler.net/problem=81
module ProjectEuler.Problem081

open System.IO
open System

let side = 80
let matrix: int[,] = Array2D.zeroCreate side side


let getData (path: string) =
    use sr = new StreamReader(path)
    let line = ref 0
    let col = ref 0
    while not sr.EndOfStream do
        col := 0
        sr.ReadLine().Split(',') |> Array.iter (fun x -> matrix.[!line, !col] <- int x; incr col)
        incr line


let problem081() =
    getData @"..\..\..\Datafiles\Problem081.data"
    for i = side - 2 downto 0 do
        matrix.[side - 1, i] <- matrix.[side - 1, i] + matrix.[side - 1, i+1]
        matrix.[i,side - 1] <- matrix.[i,side - 1] + matrix.[i+1, side - 1]

    for i = side - 2 downto 0 do
        for j = side - 2 downto 0 do
            matrix.[i, j] <- matrix.[i, j] + (min matrix.[i + 1, j] matrix.[i, j + 1])

    matrix.[0, 0]