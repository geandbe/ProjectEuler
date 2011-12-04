// Project Euler Problem 54
// http://projecteuler.net/problem=54
module ProjectEuler.Problem054

// Poker DOES NOT RANK suits (http://en.wikipedia.org/wiki/High_card_by_suit_(poker)A)
// Tie breaking rules: http://www.pokerhands.com/poker_hand_tie_rules.html 
open System
open System.IO

type Ranks = HighCard = 0 | Pair = 5 | ThreeOfAKind = 7 | Straight = 8
                | Flush = 9 | FourOfAKind = 14 | StraightFlush = 15

type Suit = Spades = 0 | Diamonds = 1 | Clubs  = 2 | Hearts = 3

type PlayingCard =
    | Ace of int*Suit | King of int*Suit | Queen of int*Suit | Jack of int*Suit | ValueCard of int*Suit

    static member Parse (code: string) =
        let suit = function
            | 'H' -> Suit.Hearts
            | 'D' -> Suit.Diamonds
            | 'S' -> Suit.Spades
            | 'C' -> Suit.Clubs
            | _ as c -> failwith "Invalid suit code: %s" c
             
        let split = code.ToCharArray()
        if split.Length <> 2 then
            failwith ("Invalid card code: " + code)

        match split.[0] with
        | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as v -> ValueCard((int)v - (int)'1', suit(split.[1]))
        | 'T' -> ValueCard(9, suit(split.[1]))
        | 'J' -> Jack(10, suit(split.[1]))
        | 'Q' -> Queen(11, suit(split.[1]))
        | 'K' -> King(12, suit(split.[1]))
        | 'A' -> Ace(13, suit(split.[1]))
        | _ -> failwith ("Invalid card code: " + code)

    member __.Value =
        match __ with
        | ValueCard(x,y) | Queen(x,y) | Jack(x,y) | King(x,y) | Ace(x,y) -> x

    member __.Suit =
        match __ with
        | ValueCard(x,y) | Queen(x,y) | Jack(x,y) | King(x,y) | Ace(x,y) -> y

let rank hand =
    let cards = hand |> Array.map PlayingCard.Parse |> Array.sortBy (fun x -> x.Value)
    let handMap: int [,] = Array2D.zeroCreate 4 13
    let suits, values, ranks = (Array.zeroCreate 4), (Array.zeroCreate 13), (Array.zeroCreate 16)

    for card in cards do handMap.[int card.Suit, card.Value - 1] <- 1

    for i in [0..3] do
        for j in [0..12] do
            suits.[i] <- suits.[i] + handMap.[i,j]
            values.[j] <- values.[j] + handMap.[i,j]
    
    if Array.max suits = 5 then // all are the same suit
        if cards.[4].Value - cards.[0].Value = 4 then // ... and consecutive value
            ranks.[int Ranks.StraightFlush] <- cards.[0].Value
        else
            for i in [0..4] do ranks.[int Ranks.Flush + i] <- cards.[i].Value
    elif Array.max values = 1 && cards.[4].Value - cards.[0].Value = 4 then
        ranks.[int Ranks.Straight] <- cards.[0].Value
    else
        let singleIdx = ref (int Ranks.HighCard)
        let pairIdx = ref (int Ranks.Pair)
        for i in [0..12] do
            match values.[i] with
            | 4 -> ranks.[int Ranks.FourOfAKind] <- i + 1
            | 3 -> ranks.[int Ranks.ThreeOfAKind] <- i + 1
            | 2 -> ranks.[!pairIdx] <- i + 1; incr pairIdx
            | 1 -> ranks.[!singleIdx] <- i + 1; incr singleIdx
            | _ -> ()
    
    ranks |> Array.fold (fun (sum, power) x -> (sum + (bigint x)*power, power*14I)) (0I, 1I) |> fst

let problem054 () =
    let readHands (path: string)  =
        seq {
            use reader = new StreamReader(path)
            let (hand1: string[]), (hand2: string[]) = (Array.zeroCreate 5), (Array.zeroCreate 5)
            while not reader.EndOfStream do
                let hands = reader.ReadLine().Split(' ')
                Array.blit hands 0 hand1 0 5; Array.blit hands 5 hand2 0 5
                yield if rank hand1 > rank hand2 then 1 else 0
        }
    
    readHands @"..\..\..\DataFiles\Problem054.data" |> Seq.sum