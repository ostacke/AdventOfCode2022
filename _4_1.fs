module _4_1
open System
let inputFP = "../../../Inputs/_4_1.txt"

let hasOverlap' (a, b, c, d) =
    not (d < a || c > b)

let hasOverlap o =
    match o with
    | None -> false
    | Some t -> hasOverlap' t

let extractSpans (s : string) : (int * int * int * int) option =
    match s.Split(',', 2) |> List.ofArray with
    | t1 :: (t2 :: []) ->
        match (t1.Split('-', 2) |> List.ofArray, t2.Split('-', 2) |> List.ofArray) with
        | (a :: (b :: []), c :: (d :: [])) -> Some (int a, int b, int c, int d)
        | _ -> 
            printfn "Unexpected input in second split of extractSpans: %s" s
            None
    | _ ->
        printfn "Unexpected input in first split of extractSpans: %s" s
        None

let solution =
    List.ofSeq(IO.File.ReadLines(inputFP))
    |> List.map (extractSpans >> hasOverlap)
    |> List.where (fun x -> x)
    |> List.length
    |> printfn "%i"