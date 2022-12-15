module _2_1
open System

let inputFP = "../../../Inputs/_2_1.txt"

let input: string list =
    IO.File.ReadLines(inputFP)
    |> List.ofSeq

/// Euclidean remainder, the proper modulo operation
let inline (%!) a b = (a % b + b) % b

let rpsHand hand =
    match hand with
    | 'A' | 'X' -> 0 // Rock
    | 'B' | 'Y' -> 1 // Paper
    | 'C' | 'Z' -> 2 // Scissors
    | _ -> -1

let rpsHands hand1 hand2 =
    (rpsHand hand1, rpsHand hand2)

let playGame (theirs, ours) =
    match (theirs - ours) %! 3 with
    | 2 -> 6
    | 1 -> 0
    | _ -> 3

let handsFromString' (c1, c2) =
    match c1 with
    | 'A' -> 
        match c2 with
        | 'X' -> (0, 2)
        | 'Y' -> (0, 0)
        | 'Z' -> (0, 1)
    | 'B' -> 
        match c2 with
        | 'X' -> (1, 0)
        | 'Y' -> (1, 1)
        | 'Z' -> (1, 2)
    | 'C' -> 
        match c2 with
        | 'X' -> (2, 1)
        | 'Y' -> (2, 2)
        | 'Z' -> (2, 0)

let handsFromString (s: string) =
    (List.head (List.ofArray (s.ToCharArray())), List.head(List.rev(List.ofArray (s.ToCharArray()))))
    |> handsFromString'

let games =
    List.ofSeq(IO.File.ReadLines(inputFP))
    |> List.map handsFromString

let getPoints (a,b) =
    (playGame (a, b)) + b + 1

let getPointsList games =
    List.map getPoints games

let solution =
    getPointsList games
    |> List.sum
    |> printf "%i"