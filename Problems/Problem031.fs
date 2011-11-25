// Project Euler Problem 31
// http://projecteuler.net/problem=31
module ProjectEuler.Problem031

// Generator of change variants for any sum and set of coin denominations
let rec change sum coins =
    if sum = 0 then [[]]
    else 
        match coins with
        | h::t -> 
            let xs = change sum t
            if sum >= h then
                let ys = change (sum - h) coins
                         |> List.map (fun xs -> h :: xs)
                List.append xs ys
            else
                xs
        | [] -> []
        
let problem031 () =
    change 200 [200;100;50;20;10;5;2;1]
    |> List.length
