module _1_2

let sumTop3 xs =
    match xs with
    | x1::(x2::(x3::xss)) -> x1 + x2 + x3
    | _ -> -1
    
let solution =
    sumTop3 _1_1.totCalsSorted
    |> printf "%i"