module _3_2
open System

let getPrio x =
    match x with
    | None ->
        printfn "%s" "getPrio of None"
        0
    | Some c ->
        let num = int c
        match Char.IsUpper c with
        | true -> num - 38
        | false -> num - 96

let rec commonChar' (l : char list) (s1 : string) (s2 : string) =
    match l with
    | [] -> None
    | c :: cs ->
        match (s1.Contains c, s2.Contains c) with
        | (true, true) -> Some c
        | _ -> commonChar' cs s1 s2

let commonChar ((s1, s2, s3) : string * string * string): char option =
    s1.ToCharArray()
    |> List.ofArray
    |> commonChar'
    <| s2 <| s3

let rec splitBy3 (ss : string list) : (string * string * string) list =
    match ss with
    | s1 :: (s2 :: (s3 :: s)) ->
        (s1, s2, s3) :: (splitBy3 s)
    | _ -> []

let inputFP = "../../../Inputs/_3_1.txt"

let solution =
    //printfn "%s" "\n"
    //getPrio (Some 'a')
    //|> printfn "%i"
    //getPrio (Some 'Z')
    //|> printfn "%i"
    //match commonChar ("abc", "defc", "ghijklmc") with
    //| None -> printfn "%s" "No match"
    //| Some c -> printfn "%c" c
    List.ofSeq(IO.File.ReadLines(inputFP))
    |> splitBy3
    |> List.map (commonChar >> getPrio)
    |> List.sum
    |> printfn "%i"