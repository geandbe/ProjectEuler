// Project Euler Problem 82
// http://projecteuler.net/problem=82
module ProjectEuler.Problem082

open System.IO
open System

let side = 80
let matrix: int[,] = Array2D.zeroCreate side side
let shortest: int[] = Array.zeroCreate side


let getData (path: string) =
    use sr = new StreamReader(path)
    let line = ref 0
    let col = ref 0
    while not sr.EndOfStream do
        col := 0
        sr.ReadLine().Split(',') |> Array.iter (fun x -> matrix.[!line, !col] <- int x; incr col)
        incr line


let problem082() =
    getData @"..\..\..\Datafiles\Problem081.data" // same data P.81 & 82
    for i = 0 to side - 1 do
        shortest.[i] <- matrix.[i, side - 1]

    for i = side - 2 downto 0 do
        shortest.[0] <- shortest.[0] + matrix.[0, i]
        for j = 1 to side - 1 do
            shortest.[j] <- min (shortest.[j - 1] + matrix.[j, i]) (shortest.[j] + matrix.[j, i])
        for j = side - 2 downto 0 do
            shortest.[j] <- min shortest.[j] (shortest.[j + 1] + matrix.[j, i])

    Array.min shortest

