// Project Euler Problem 39
// http://projecteuler.net/problem=39
module ProjectEuler.Problem039

let pythagoreanTriplesNo perimeter =
    [for a = 1 to perimeter/3 do
        for b = a to (perimeter - a)/2 do
            let c = perimeter - a - b
            if a*a + b*b = c*c then
                yield (a, b, c)]
    |> List.length

let problem039 () =
    [3..1000]
    |> List.map (fun x -> (x,(pythagoreanTriplesNo x)))
    |> List.maxBy (snd)
    |> (fst)