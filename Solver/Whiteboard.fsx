open System
open System.Linq

[<Literal>]
let SIZE = 10

let vector : int[] = Array.zeroCreate SIZE

let matrix : int[,] = Array2D.zeroCreate SIZE SIZE

let idxs = Enumerable.Range(0, SIZE).Select(fun i -> vector.[i] <- 1)
let idxs' = Enumerable.Range(0, SIZE).Select(fun i -> printfn "%d" i)

for i in Enumerable.Range(0, 10) do (fun i -> printfn "%d" i)(i)

[32..-1..0]
for i in Enumerable.Range(0, 10).OrderByDescending(fun x -> x) do (fun i -> printfn "%d" i)(i)