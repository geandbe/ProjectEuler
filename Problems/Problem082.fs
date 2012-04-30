// Project Euler Problem 82
// http://projecteuler.net/problem=82
module ProjectEuler.Problem082

open System.IO

[<Literal>]
let SIDE = 80
let matrix: int[,] = Array2D.zeroCreate SIDE SIDE
let shortest: int[] = Array.zeroCreate SIDE

let getData (path: string) =
    use sr = new StreamReader(path)
    let line, col = ref 0, ref 0
    while not sr.EndOfStream do
        col := 0
        sr.ReadLine().Split(',') |> Array.iter (fun x -> matrix.[!line, !col] <- int x; incr col)
        incr line

let problem082() =
    getData @"..\..\..\Datafiles\Problem081.data" // same data P.81 & 82
    List.iter (fun row -> shortest.[row] <- matrix.[row, SIDE-1]) [0..SIDE-1]
    List.iter (fun col ->
        shortest.[0] <- shortest.[0] + matrix.[0, col]
        List.iter (fun i -> shortest.[i] <- matrix.[i, col] + min shortest.[i-1] shortest.[i]) [1..SIDE-1]
        List.iter (fun i -> shortest.[i] <- min shortest.[i] (shortest.[i+1] + matrix.[i, col])) [SIDE-2..-1..0]
        ) [SIDE-2..-1..0]
    Array.min shortest

