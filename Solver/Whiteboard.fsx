// http://www.policyalmanac.org/games/aStarTutorial.htm  - A* algo description

#if INTERACTIVE
#r @"C:\Program Files (x86)\FSharpPowerPack-2.0.0.0\bin\FSharp.PowerPack.dll"
#endif

open Microsoft.FSharp.Collections
open System.Collections.Generic
open System
open System.IO

[<Literal>]
let SIDE = 80
let matrix: int[,] = Array2D.zeroCreate SIDE SIDE

let getData (path: string) =
    use sr = new StreamReader(path)
    let line, col = ref 0, ref 0
    while not sr.EndOfStream do
        col := 0
        sr.ReadLine().Split(',') |> Array.iter (fun x -> matrix.[!line, !col] <- int x; incr col)
        incr line

//getData @"..\..\..\Datafiles\Problem081.data" // same data P.81 & 82
getData @"C:\Users\Gene\Projects\ProjectEuler\Datafiles\Problem081.data" // same data P.81 & 82

type Coord = int*int

let findMinPath (matrix: int[,]) =
    let openList = new HashMultiMap<_,_>(HashIdentity.Structural)
    let closeList = HashSet<Coord>()
    openList.Add(matrix.[0, 0], (0,0))

    let rec step () =
        if openList.Count = 0 then failwith "No path exists"
        let minKey = openList.Fold (fun key _ acc -> if key < acc then key else acc) Int32.MaxValue
        let minElem = openList.FindAll minKey |> List.head
        let cost, node = minKey, minElem
        openList.Remove minKey
        let x,y = node
        [ (x-1,y); (x+1,y); (x,y-1); (x, y+1) ] // reachable from node
        |> List.filter (fun (x,y) -> x >= 0 && y >=0 && x < SIDE && y < SIDE)
        |> List.iter (fun coords ->
            if not (closeList.Contains coords) then
                    openList.Add(cost + matrix.[fst coords, snd coords], coords))
        closeList.Add(node) |> ignore
        if node = (SIDE-1,SIDE-1) then cost else step()

    step()

findMinPath matrix |> printfn "%d"

