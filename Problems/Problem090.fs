// Project Euler Problem 90
// http://projecteuler.net/problem=90
module ProjectEuler.Problem090

let squares =
    [ for i in 1..9 -> i*i ]
    |> List.map (fun square -> let tens, ones = square/10, square%10 in
                                (tens, if ones = 9 then 6 else ones))

let isValidArrangement dice1 dice2 = 
    let rec scan = function
    | [] -> true
    | (tens,ones)::t -> if (Set.contains tens dice1 && Set.contains ones dice2) ||
                           (Set.contains tens dice2 && Set.contains ones dice1) then scan t
                        else false
    scan squares

let rec combinations size ls =
    let rec bead = function
    | [] -> []
    | h::t -> (h,t) :: [ for (l, ls) in bead t -> (l, ls) ]
    [match size with
     | 0 -> yield []
     | _ -> for (first, rest) in bead ls do
              for tail in combinations (size - 1) rest do
                  yield first::tail]                

let allDices = combinations 6 [0..9]

let asSetofSides dice =
    let sides = Set.ofList dice in
        if sides.Contains 9 && not (sides.Contains 6)
        then (sides.Remove 9).Add 6
        else sides

let allDicePairs = seq {
    for dice1 in allDices do
        for dice2 in allDices do
            yield (asSetofSides dice1, asSetofSides dice2)
    }

let problem090() =
    allDicePairs
    |> Seq.sumBy (fun (dice1, dice2) -> if isValidArrangement dice1 dice2 then 1 else 0)
    |> (/) <| 2