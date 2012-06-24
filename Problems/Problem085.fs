// Project Euler Problem 85
// http://projecteuler.net/problem=85
module ProjectEuler.Problem085

let squares x y = x * (x + 1) * y * (y + 1) / 4
let solutions = Seq.unfold(fun y ->
                let x = int(sqrt(float (8000000/(y*(y+1)))))
                let solution = if squares x y > 2000000 then (x - 1,y) else (x,y)
                if fst solution <= y then None else Some(solution, y + 1)) 1

let problem085() =
    solutions |> Seq.toList
    |> List.map (fun (x,y) -> (2000000 - (squares x y), x*y))
    |> List.sortBy (fun (x,y) -> x) |> List.head |> snd