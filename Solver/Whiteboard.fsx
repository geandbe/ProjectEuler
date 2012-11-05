open System
open System.Runtime.InteropServices

let a = 1u : uint32

module Test =
    let x = 3
    
    [<DllImport("kernel32.dll", SetLastError=true)>]
    extern bool private Beep(uint32 dwFreq, uint32 dwDuration);

    let private a = 1
    let b = 1

    
module Gravatar =
    open System
    open System.Text
    open System.Security.Cryptography

    let email2hash (email: string) =
        use md5provider = MD5.Create()
        md5provider.ComputeHash(Encoding.UTF8.GetBytes(email))
        |> Array.fold (fun result (b: byte) -> result + b.ToString("x2")) ""

    let gravatarURI email =
        @"http://www.gravatar.com/avatar/" + (email2hash email) + @"/?d=identicon"
//======================================================================================================

let K = 12000
let K2 = K*2

module List =
    let mult xs =
        xs |> List.reduce (fun a x -> a * x)

let rec moveNext ls =
    if List.length ls = 14 then
        let h = List.head ls
        let t = List.tail ls
        if (h + 1) * (List.mult t) <= K2 then
            (h + 1) :: t
        else moveNext t
    elif ls = [] then
        ls
    else
        let h = List.head ls
        let t = List.tail ls
        let mutable candidate = []
        for i in 1 .. (14 - t.Length) do
            candidate <- (h + 1)::candidate
        candidate <- candidate @ t
        if (List.mult candidate) <= K2 then
            candidate
        else
            moveNext t

let continueOK ls =    
    ls <> []
    
let prodSums = Seq.unfold (fun state -> if (not (continueOK state)) then None else Some(state, moveNext(state))) [2;2;1;1;1;1;1;1;1;1;1;1;1;1]
let result = prodSums |> Seq.map (fun x -> List.filter (fun l -> l<>1) x)  |> Seq.toList
printfn "%A" result

