// Project Euler Problem 75
// http://projecteuler.net/problem=75

module ProjectEuler.Problem075

// Euclidean algorithm for gcd(n,m)
let rec gcd n m =
    match m with
    | 0 -> n
    | _ -> gcd m (n % m)

let maxPerimeter = 1500000
let maxSide = maxPerimeter/2 |> (float >> sqrt >> int)

// Euclid's formula for generating Pythagorean triples
// (http://en.wikipedia.org/wiki/Pythagorean_triple) 
let perimeters =
    let multiples p =
        Seq.unfold (fun perimeter ->
            if perimeter <= maxPerimeter
            then Some(perimeter, perimeter + p) else None) p
        
    
    [ for m in 2..maxSide do
        for n in 1..(m - 1) do
            if (n + m) % 2 = 1 && (gcd n m = 1) then
                yield! multiples (2 * (m*m + n*m))
    ]

let problem075() =
    perimeters
    |> Seq.groupBy id
    |> Seq.fold (fun sum (x,y) ->
        if Seq.length y = 1 then sum + 1 else sum) 0

