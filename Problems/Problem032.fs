// Project Euler Problem 32
// http://projecteuler.net/problem=32
module ProjectEuler.Problem032

//-------------prelim estimation-------------------------------
let maxNum digits = pown 10 digits - 1
let minNum digits = pown 10 (digits - 1) 
let nDigits num = int (log(float num) / log(10.)) + 1
let splits9into3 =
    [
        for a = 1 to 9 do
            for b = 1 to 9 do
                for c = 1 to 9 do
                    if a + b + c = 9 then
                        yield [a;b;c]
    ]

let isCandidate split =
    let [a;b;c] = split
    nDigits ((maxNum a) * (maxNum b)) > c && nDigits ((minNum a) * (minNum b)) < c 

splits9into3 |> List.filter isCandidate |> List.iter (printfn "%A")
//-------------------------------------------------------------

let isPandigitalProduct (a: int) (b: int) =
    let isPandigitalString (s: string) =
        let ss = s.ToCharArray() |> Set.ofArray
        s.Length = 9 && Set.count ss = 9 && ss.MinimumElement > '0'

    isPandigitalString (sprintf "%d%d%d" a b (a*b))

let problem032 () =
    [
        for i = 2 to 99 do
            for j = i to 9999 do
                if isPandigitalProduct i j then
                    yield (i*j)
    ]
    |> Set.ofList |> Set.toList |> List.sum