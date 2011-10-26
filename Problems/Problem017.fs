// Project Euler Problem 17
// http://projecteuler.net/problem=17
module ProjectEuler.Problem017

open System.Text

let inline glue (sb: StringBuilder) (perm: string) (piece: string) =
    sb.Append(sprintf "%s%s" piece perm) |> ignore

// n -> [|thousands; hundreds; tens; ones|]
let asPositional n =
    Seq.unfold (fun (x,y) ->
        if y >= 1 then
            Some (x/y, (x%y, y/10))
        else None)
        (n,1000)
    |> Seq.toArray
    
let tens =
    [""; ""; "twenty"; "thirty"; "forty";
    "fifty"; "sixty"; "seventy"; "eighty"; "ninety"]
let teens =
    ["ten"; "eleven"; "twelve"; "thirteen"; "fourteen";
    "fifteen"; "sixteen"; "seventeen"; "eighteen"; "nineteen"]
let ones =
    [""; "one"; "two"; "three"; "four";
    "five"; "six"; "seven"; "eight"; "nine"]

let asWords (num: int array) =
    let sb = StringBuilder()
    if num.[0] <> 0 then
        glue sb " thousand" ones.[num.[0]]
        if num.[1] = 0 && (num.[2] <> 0 || num.[3] <> 0) then glue sb " and " ""
    if num.[1] <> 0 then
        glue sb " hundred" ones.[num.[1]]
        if num.[2] <> 0 || num.[3] <> 0 then glue sb " and " ""
    if num.[2] = 1 then
        glue sb "" teens.[num.[3]]
    else
        if num.[2] >= 2 then
            glue sb "" tens.[num.[2]]
        glue sb "" ones.[num.[3]]
    sb.ToString()

let lettersCount (numAsWords: string) =
    numAsWords.Replace (" ","") |> String.length

let problem017 () =
    [1..1000]
    |> List.map (asPositional >> asWords >> lettersCount)
    |> solve PassAll PassAll Seq.sum