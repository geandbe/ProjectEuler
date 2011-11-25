// Project Euler Problem 33
// http://projecteuler.net/problem=33
module ProjectEuler.Problem033

let problem033 () =
    [for x = 1 to 9 do
        for y = 1 to 9 do
            for z = 1 to 9 do
                if 10*z*(x - y) = y * (x - z) && y > z then
                    //printfn "%d/%d" (10*z + x) (10*x + y)
                    yield (10*z + x), (10*x + y)
    ]
    |> Seq.fold (fun (numerator, denominator) (x,y) ->
                    (numerator*x, denominator*y)) (1,1)
    |> fun (numerator,denominator) -> denominator/numerator