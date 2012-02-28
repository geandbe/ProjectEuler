// Project Euler Problem 80
// http://projecteuler.net/problem=80

module ProjectEuler.Problem080

let sumOfDigits (n: bigint) =
    n.ToString().ToCharArray()
    |> Array.map (fun x -> (int)x - (int)'0') |> Array.sum

let rec SqRt v =
    let ``10**99`` = BigNum.PowN (10N, 99)
    let ``10**100`` = ``10**99`` * 10N
    let err = 1N/``10**100``
    let rec nextX x =
        let x' = (x + v/x)/2N
        if abs(x' - x) > err then nextX x'
        else (BigNum.ToBigInt (x' * ``10**99``))
    nextX v

let problem080 () =
    (set [2..99] - set [4;9;16;25;36;49;64;81])
    |> Set.toList |> List.fold (fun s x -> s + sumOfDigits (SqRt (BigNum.FromInt x))) 0