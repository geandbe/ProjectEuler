// Project Euler Problem 83
// http://projecteuler.net/problem=83
module ProjectEuler.Problem083

open System
open System.IO
open Microsoft.FSharp.Collections
open System.Collections.Generic

[<Literal>]
let SIDE = 80
let matrix: int[,] = Array2D.zeroCreate SIDE SIDE

let getData (path: string) =
    use sr = new StreamReader(path)
    let line, col = ref 0, ref 0
    while not sr.EndOfStream do
        col := 0
        sr.ReadLine().Split(',')
        |> Array.iter (fun x -> matrix.[!line, !col] <- int x; incr col)
        incr line

let findMinPath (matrix: int[,]) =
    let openList = new HashMultiMap<_,_>(HashIdentity.Structural)
    let closeList = HashSet<_>()
    openList.Add(matrix.[0, 0], (0,0))
    let rec step () =
        if openList.Count = 0 then failwith "No path exists"
        let minKey = openList.Fold (fun key _ acc ->
                if key < acc then key else acc) Int32.MaxValue
        let minElem = openList.FindAll minKey |> List.head
        let cost, (x,y) = minKey, minElem
        openList.Remove minKey
        [ (x-1,y); (x+1,y); (x,y-1); (x, y+1) ] // reachable from node
        |> List.filter (fun (x,y) -> x >= 0 && y >=0 && x < SIDE && y < SIDE)
        |> List.iter (fun (x,y) ->
            if not (closeList.Contains (x,y)) then
                    openList.Add(cost + matrix.[x, y], (x,y)))
        closeList.Add(x,y) |> ignore
        if (x,y) = (SIDE-1,SIDE-1) then cost else step()
    step()

let problem083() =
    getData @"..\..\..\Datafiles\Problem081.data" // same data P.81 & 82 & 83
    findMinPath matrix
