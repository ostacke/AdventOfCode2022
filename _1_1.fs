module _1_1
open System

let inputFP = "../../../Inputs/_1_1.txt"

let rec splitByEmptyString' (newLs:list<list<string>>) (oldLs:list<string>) :list<list<string>> =
    match oldLs with
    | [] -> newLs
    | ""::xs -> splitByEmptyString' ([]::newLs) xs
    | x::xs ->
        match newLs with
        | [] -> splitByEmptyString' [[x]] xs
        | y::ys -> splitByEmptyString' ((x::y)::ys) xs

let splitByEmptyString (ls:list<string>) : list<list<string>> =
    let isNotEmpty xs =
        match xs with
        | [] -> false
        | _ -> true
    splitByEmptyString' [] ls
    |> List.filter isNotEmpty

let totCalsSorted =
    List.ofSeq(IO.File.ReadLines(inputFP))
    |> splitByEmptyString
    |> List.map (fun xs -> List.fold (fun x y -> x + (int y)) 0 xs)
    |> List.sort
    |> List.rev

let solution =
    totCalsSorted
    |> List.head
    |> printfn "%i"