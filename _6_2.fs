module _6_2
open System
let inputFP = "../../../Inputs/_6_1.txt"

let rec lockOn (i : int) (frameWidth : int) (cs : char list) : int =
    match cs |> List.take frameWidth |> Seq.distinct |> Seq.length with
    | x when x = frameWidth -> i
    | _ -> cs |> List.tail |> lockOn (i + 1) frameWidth

let solution = 
    (List.ofSeq(IO.File.ReadLines(inputFP))[0]).ToCharArray()
    |> List.ofArray
    |> lockOn 14 14
    |> printfn "%i"