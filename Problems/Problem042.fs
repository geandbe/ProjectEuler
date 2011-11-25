// Project Euler Problem 42
// http://projecteuler.net/problem=42
module ProjectEuler.Problem042

open System.IO

let isTriangleNumber n = 
    let product = n*2
    let candidate = int (floor (sqrt (float product)))
    candidate * (candidate + 1) = product

let wordValue (word: string) =
    let baseValue = int '@'
    word |> Seq.fold (fun wval letter ->
                wval + int letter - baseValue) 0

let readData (path: string) =  
    use sr = new StreamReader(path)    
    sr.ReadLine().Split(',')
    
let problem042 () =
    readData @"..\..\..\Datafiles\Problem042.data"
    |> Array.map (fun x -> x.Replace("\"",""))  
    |> Array.map wordValue
    |> Array.filter isTriangleNumber
    |> Array.length