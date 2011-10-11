open ProjectEuler

let stub () = None

let problems : (unit -> obj option) array =
    [|
        stub; //problem001 >> box >> Some;
        stub; //problem002 >> box >> Some;
        stub; //problem003 >> box >> Some;
        stub; //problem004 >> box >> Some;
        stub; //problem005 >> box >> Some;
        stub; //problem006 >> box >> Some;
        stub; //problem007 >> box >> Some;
        problem008 >> box >> Some;
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