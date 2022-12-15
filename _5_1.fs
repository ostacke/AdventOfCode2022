module _5_1
open System
let inputFP = "../../../Inputs/_5_1.txt"

type craneOp = {
        Count: int
        From: int
        To: int
    }

// Region: Calc result
let top (stack : char list) : char option =
    match stack with
    | c :: cs -> Some c
    | _ -> None

let rec tops' (ls : char option list) =
    match ls with
    | (Some c) :: cs -> c :: (tops' cs)
    | None :: cs -> tops' cs
    | [] -> []

let tops (stacks : char list list) : string =
    List.map top stacks
    |> tops'
    |> List.map string
    |> List.reduce (+)
// End region

// Region: Perform operation
let getFrom (cs : char list) (count : int) : char list * char list =
    List.splitAt count cs

let rec applyCraneOp1 (op : craneOp) (state : char list list) : char list list =
    match op with
    | {Count=0;From=_;To=_} -> state
    | {Count=c;From=f;To=t} ->
        let (toBeMoved, leftBehind) = state[op.From - 1] |> List.splitAt 1
        state |> List.indexed
        |> List.map (fun (k,v) ->
            if (k + 1) = op.From then leftBehind
            elif (k + 1) = op.To then List.append toBeMoved v
            else v)
        |> applyCraneOp1 {Count = c - 1; From = f; To = t}

let applyCraneOp2 (op : craneOp) (state : char list list) : char list list =
    let (toBeMoved, leftBehind) = state[op.From - 1] |> List.splitAt op.Count
    state |> List.indexed
    |> List.map (fun (k,v) ->
        if (k + 1) = op.From then leftBehind
        elif (k + 1) = op.To then List.append toBeMoved v
        else v)

let rec applyCraneOps (ops : craneOp list) (state : char list list) : char list list =
    match ops with
    | [] -> state
    | o :: os -> applyCraneOp2 o state |> applyCraneOps os
// End region

// Region: Get starting state and instructions
let rec transpose xs = [
  match xs with
  | [] -> failwith "cannot transpose a 0-by-n matrix"
  | []::xs -> () 
  | xs -> 
      yield List.map List.head xs 
      yield! transpose (List.map List.tail xs) ]

let getState' (ss : string list) : char list list =
    ss |> List.map (fun s ->
        s.ToCharArray()
        |> List.ofArray
        |> List.indexed
        |> List.where (fun (i, _) -> (i % 4) = 1)
        |> List.map snd)
    |> transpose
    |> List.map (fun cs -> List.where (fun c -> Char.IsLetter(c)) cs)

let getState (ss : string list) : char list list =
    List.rev ss |> List.tail |> List.rev |> getState'

let getOps (ss : string list) : craneOp list =
    ss |> List.map (fun s ->
        let words = s.Split(' ')
        {Count=(int words[1]); From=(int words[3]); To=(int words[5])})

let getInput : char list list * craneOp list =
    let input = List.ofSeq(IO.File.ReadLines(inputFP))
    let stateStrings = List.takeWhile (fun x -> not (x = "")) input
    let opsStrings = List.rev input |> List.takeWhile (fun x -> not (x = "")) |> List.rev
    (getState stateStrings, getOps opsStrings)
// End region

let implode (xs:char list) =
    let sb = Text.StringBuilder(xs.Length)
    xs |> List.iter (sb.Append >> ignore)
    sb.ToString()

let solution =
    match getInput with
    | (state, ops) ->
        applyCraneOps ops state
        |> List.map (fun x -> x[0])
        |> implode
        |> printf "%s"
    //match getInput with
    //| (state, ops) ->
    //    ops
    //    |> List.map (fun op ->
    //        printf "%i" op.Count
    //        printf "%i" op.From
    //        printfn "%i" op.To
    //        )
    //match getInput with
    //| (state, ops) ->
    //    state   
    //    |> List.map implode
    //    |> List.map (fun s -> printfn "%s" s)