module _7_2
open System
let inputFP = "../../../Inputs/_7_1.txt"

type Filesystem =
    | Dir of string * int * Filesystem seq 
    | File of int * string

type Command =
    | Cd of string
    | Ls

type OutputLine =
    | FileLine of int * string
    | DirLine of string

type ConsoleLine =
    | Command of Command
    | Output of OutputLine

let mkCl (ls : string list) : ConsoleLine =
    match ls with
    | "$" :: "cd" :: name :: [] -> Command (Cd name)
    | "$" :: "ls" :: [] -> Command Ls
    | "dir" :: name :: [] -> Output (DirLine name)
    | size :: name :: [] -> Output (FileLine (int size, name))
    | _ -> failwith "error on line"

let rec mkClList' (lss : string list list) : ConsoleLine list = 
    match lss with
    | [] -> []
    | ls :: lss -> (mkCl ls) :: mkClList' lss

let mkClList (ls : string list) : ConsoleLine list =
    ls |> List.map (fun s -> s.Split(' ') |> List.ofArray)
    |> mkClList'

let rec addToPath' (state : Filesystem) (x : Filesystem) (path : string list) : Filesystem =
    match (state, path) with
    | (Dir (name, size, contents), d :: []) when d = name -> Dir (name, size, (Seq.append contents [x]))
    | (Dir (name, size, contents), d1 :: d2 :: ds) when d1 = name ->
        let matchNextDir x =
            match x with
            | Dir (nextName, _, _) when nextName = d2 -> true
            | _ -> false
        let nextDir = contents |> Seq.find matchNextDir
        Dir (name, 0, (Seq.append (Seq.where (fun x -> x |> matchNextDir |> not) contents) [addToPath' nextDir x (d2 :: ds)]))
    | _ -> failwith "Could not add to path"

let addToPath (state : Filesystem) (x : Filesystem) (path : string list) : Filesystem =
    addToPath' state x (List.rev path)

let rec filesystem' (ls : ConsoleLine list) (state : Filesystem) (path : string list) : Filesystem = 
    match ls with
    | [] -> state
    | cl :: cls ->
        match cl with
        | Command (Cd "..") -> filesystem' cls state path.Tail
        | Command (Cd name) -> filesystem' cls state (name :: path)
        | Command Ls -> filesystem' cls state path
        | Output (FileLine (size, name)) -> filesystem' cls (addToPath state (File (size, name)) path) path
        | Output (DirLine name) -> filesystem' cls (addToPath state (Dir (name, 0, Seq.empty)) path) path

let filesystem ls =
    match ls with
    | (Command (Cd root)) :: cls when root = "/" -> filesystem' cls (Dir (root, 0, Seq.empty)) [root]
    | _ -> failwith "Wrong start of comand lines"

let rec size (fs : Filesystem) : Filesystem =
    match fs with
    | Dir (name, _, contents) ->
        let newContents = Seq.map size contents
        let newSize = newContents |> Seq.sumBy (fun x -> match x with | Dir (_,s,_) -> s | File (s,_) -> s)
        Dir (name, newSize, newContents)
    | File (a,b) -> File (a,b)

let rec findClosestAbove (lim : int) ((currName, currSize) : string * int) (fs : Filesystem) : string * int =
    match fs with
    | Dir (name,size,cs) when size >= lim && size < currSize ->
        Seq.fold (findClosestAbove lim) (name, size) cs
    | Dir (_,_,cs) ->
        Seq.fold (findClosestAbove lim) (currName, currSize) cs
    | _ -> currName, currSize
    
let totalDiskSpace = 70000000
let needsToBeFree = 30000000
let leftOnDisk = 41609574
let leftToBeRemoved = leftOnDisk - (totalDiskSpace - needsToBeFree)

let solution =
    List.ofSeq(System.IO.File.ReadLines(inputFP))
    |> mkClList
    |> filesystem
    |> size
    |> findClosestAbove leftToBeRemoved ("/", leftOnDisk)
    |> snd
    |> printfn "%i"
