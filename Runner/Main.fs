open ProjectEuler

let stub () = None

let problems : (unit -> obj option) array =
    [|
        problem001;
        problem002;
        problem003;
        stub;
        stub;
        stub;
        stub;
        stub;
        stub;
        stub;
    |]


[<EntryPoint>]
let main (args: string[]) =
    for i = 0 to problems.Length - 1 do
        match problems.[i]() with
        | None -> printfn "Problem %d is excluded or not implemented yet" (i + 1)
        | Some(_) as result -> printfn "Project Euler Problem %d Answer: %A" (i + 1) result.Value
    0