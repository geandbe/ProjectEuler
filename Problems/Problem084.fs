// Project Euler Problem 84
// http://projecteuler.net/problem=84
module ProjectEuler.Problem084
let [<Literal>]BOARDSIZE = 40
let [<Literal>]CCSIZE = 16
let [<Literal>]CHANCESIZE = 16
let [<Literal>]DICESIDES = 4

let [<Literal>]GO = 0
let [<Literal>]CC1 = 2
let [<Literal>]R1 = 5
let [<Literal>]CH1 = 7
let [<Literal>]JAIL = 10
let [<Literal>]C1 = 11
let [<Literal>]U1 = 12
let [<Literal>]R2 = 15
let [<Literal>]CC2 = 17
let [<Literal>]CH2 = 22
let [<Literal>]E3 = 24
let [<Literal>]R3 = 25
let [<Literal>]U2 = 28
let [<Literal>]G2J = 30
let [<Literal>]CC3 = 34
let [<Literal>]CH3 = 36
let [<Literal>]H2 = 39

let rand = System.Random()
let board = Array.zeroCreate<int> BOARDSIZE

type Doubles = ZeroDouble | OnceDouble | TwiceDouble

let nextRoll() = (rand.Next(DICESIDES) + 1, rand.Next(DICESIDES) + 1)

let (|Double|Ordinary|) roll =
    let sum = fst roll + snd roll in
    if fst roll = snd roll then Double sum else Ordinary sum

let forward boardPosition score = (boardPosition + score) % BOARDSIZE
    
let inc square =
    board.[square] <- board.[square] + 1

let processChestCard (boardPosition, chestCard, chanceCard, doubles) =
    let newChestCard = (chestCard + 1) % CCSIZE
    match newChestCard with
    | 0 -> inc GO; (GO, newChestCard, chanceCard, doubles)
    | 1 -> inc JAIL; (JAIL, newChestCard, chanceCard, doubles)
    | _ -> inc boardPosition; (boardPosition, newChestCard, chanceCard, doubles)

let ifChest (boardPosition, chestCard, chanceCard, doubles) =
    match boardPosition with
    | CC1 | CC2 | CC3 -> processChestCard (boardPosition, chestCard, chanceCard, doubles)
    | G2J -> inc JAIL; (JAIL, chestCard, chanceCard, doubles)
    | _ -> inc boardPosition; (boardPosition, chestCard, chanceCard, doubles)

let processChanceCard (boardPosition, chestCard, chanceCard, doubles) =
    let newChanceCard = (chanceCard + 1) % CHANCESIZE
    match newChanceCard with
    | 0 -> ifChest (GO, chestCard, newChanceCard, doubles)
    | 1 -> ifChest (JAIL, chestCard, newChanceCard, doubles)
    | 2 -> ifChest (C1, chestCard, newChanceCard, doubles)
    | 3 -> ifChest (E3, chestCard, newChanceCard, doubles)
    | 4 -> ifChest (H2, chestCard, newChanceCard, doubles)
    | 5 -> ifChest (R1, chestCard, newChanceCard, doubles)
    | 6 | 7 -> if boardPosition = CH1 then ifChest (R2, chestCard, newChanceCard, doubles)
               elif boardPosition = CH2 then ifChest (R3, chestCard, newChanceCard, doubles)
               elif boardPosition = CH3 then ifChest (R1, chestCard, newChanceCard, doubles)
               else ifChest (boardPosition, chestCard, newChanceCard, doubles)
    | 8 -> if boardPosition = CH2 then ifChest (U2, chestCard, newChanceCard, doubles)
           else ifChest (U1, chestCard, newChanceCard, doubles)
    | 9 -> ifChest (boardPosition - 3, chestCard, newChanceCard, doubles)
    | _ -> ifChest (boardPosition, chestCard, newChanceCard, doubles)

let ifChance (boardPosition, chestCard, chanceCard, doubles) =
    match boardPosition with
    | CH1 | CH2 | CH3 -> processChanceCard (boardPosition, chestCard, chanceCard, doubles)
    | _ -> ifChest (boardPosition, chestCard, chanceCard, doubles)

let rollDice (boardPosition, chestCard, chanceCard, doubles) =
    match nextRoll() with
    | Double score -> match doubles with
                      | ZeroDouble -> ifChance ((forward boardPosition score), chestCard, chanceCard, OnceDouble)
                      | OnceDouble -> ifChance ((forward boardPosition score), chestCard, chanceCard, TwiceDouble)
                      | TwiceDouble -> inc JAIL; (JAIL, chestCard, chanceCard, ZeroDouble)
    | Ordinary score -> ifChance ((forward boardPosition score), chestCard, chanceCard, ZeroDouble)
    
let problem084() =
    let _ = Seq.initInfinite id
            |> Seq.scan (fun (boardPosition, chestCard, chanceCard, doubles) x ->
                rollDice (boardPosition, chestCard, chanceCard, doubles)) (0,0,0,ZeroDouble)
            |> Seq.nth 1000000
    board
    |> Array.mapi (fun i x -> (-x,i))
    |> Array.sortBy(fun (x,y) -> x)
    |> Array.map (fun (x,y) -> y)
    |> Array.toSeq |> Seq.take 3
    |> Seq.fold (fun a x -> a + string x) ""