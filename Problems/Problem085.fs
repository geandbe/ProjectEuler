// Project Euler Problem 85
// http://projecteuler.net/problem=85
module ProjectEuler.Problem085

let rectangles w h = w * (w + 1) * h * (h + 1) / 4
let solutions =
    Seq.unfold(fun h ->
        let w = int(sqrt(float (8000000/(h*(h+1)))))
        let solution = if rectangles w h > 2000000 then (w - 1,h) else (w,h)
        if fst solution <= h then None else Some(solution, h + 1)) 1

let problem085() =
    solutions |> Seq.toList
    |> List.map (fun (w,h) -> (2000000 - (rectangles w h), w*h))
    |> List.sortBy (fun (diff,area) -> diff) |> List.head |> snd