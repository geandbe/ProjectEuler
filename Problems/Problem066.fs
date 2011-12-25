// Project Euler Problem 66
// http://projecteuler.net/problem=66
module ProjectEuler.Problem066

let floorSqrt = float >> sqrt >> int

let isSquare x =
    let sqRoot = floorSqrt x
        in sqRoot * sqRoot = x

let continuedFractionExpansion s =
    if isSquare s then
        failwithf "s = %d is a perfect square" s

    let a0 = floorSqrt s
    Seq.unfold (fun (d, m) ->
        let d = (s - m * m) / d in
        Some ((a0 + m) / d, (d, ((a0 + m) / d) * d - m)))
        (1, a0)
    |> Seq.append [a0]

let solvePellEq s = 
    continuedFractionExpansion s
    |> Seq.scan (fun (``h(n)``,``k(n)``,``h(n-1)``,``k(n-1)``) ``a(n)`` ->
                     ((bigint ``a(n)``) * ``h(n)`` + ``h(n-1)``,
                      (bigint ``a(n)``) * ``k(n)`` + ``k(n-1)``,
                      ``h(n)``,``k(n)``))
                     (1I,0I,0I,1I)
    |> Seq.skip 1
    |> Seq.find (fun (h,k,_,_) -> h*h - (bigint s)*k*k = 1I)
    |> fun (x,y,_,_) -> (x,y)
    
let problem066 () =
    (Seq.initInfinite >> Seq.skip 2) id
    |> Seq.filter (isSquare >> not)
    |> Seq.takeWhile ((>=) 1000)
    |> Seq.map (fun x -> (((solvePellEq >> fst) x), x))
    |> Seq.maxBy fst
    |> snd