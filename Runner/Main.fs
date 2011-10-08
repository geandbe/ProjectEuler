open ProjectEuler

let stub () = None

let problems : (unit -> obj option) array =
    [|
        problem001 >> box >> Some;
        problem002 >> box >> Some;
        problem003 >> box >> Some;
        problem004 >> box >> Some;
        problem005 >> box >> Some;
        problem006 >> box >> Some;
        stub;
        stub;
        stub;
        stub;
    |]


[<EntryPoint>]
let main (args: string[]) =
    for i = 0 to problems.Length - 1 do
        match problems.[i]() with
        | None -> printfn "Project Euler Problem %d is excluded or not implemented yet" (i + 1)
        | Some(_) as result -> printfn "Project Euler Problem %d Answer: %A" (i + 1) result.Value
    0