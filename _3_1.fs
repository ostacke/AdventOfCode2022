module _3_1
open System

let getPrio c =
    let num = int c
    match Char.IsUpper c with
    | true -> num - 38
    | false -> num - 96

let rec getDuplicate' (l : char list) (s : string) : char option =
    match l with
    | [] -> None
    | c :: cs ->
        match s.Contains c with
        | false -> getDuplicate' cs s
        | true -> Some(c)

let getDuplicate ((s1 : string), (s2 : string)) : char option =
    s1.ToCharArray()
    |> List.ofArray
    |> getDuplicate'
    <| s2

let halve (s : string) : string * string =
    let indexAfterHalf = s.Length / 2
    ( s.Substring( 0, indexAfterHalf), s.Substring indexAfterHalf )

let prioFromString s =
    match halve s |> getDuplicate with
    | None -> 0
    | Some c -> getPrio c

let inputFP = "../../../Inputs/_3_1.txt"

let solution =
    List.ofSeq(IO.File.ReadLines(inputFP))
    |> List.map prioFromString
    |> List.sum
    |> printfn "%i"