module _6_1
open System
let inputFP = "../../../Inputs/_6_1.txt"

// i = index of d + 2
let rec lockOn (i : int) (cs : char list) : int =
    match cs with
    | a :: b :: c :: d :: ls ->
        match [a; b; c; d] |> Seq.distinct |> Seq.length with
        | 4 -> i
        | _ -> lockOn (i + 1) (b :: c :: d :: ls)
    | _ -> failwith "Cannot lock on to signal"

let solution = 
    (List.ofSeq(IO.File.ReadLines(inputFP))[0]).ToCharArray()
    |> List.ofArray
    |> lockOn 4
    |> printfn "%i"