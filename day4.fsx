let rec digits =
    function
    | 0 -> []
    | n -> (n % 10) :: digits (n / 10)

let hasDups: int list -> bool = List.pairwise >> List.exists (fun (a, b) -> a = b)

let increasing: int list -> bool = List.pairwise >> List.forall (fun (a, b) -> a >= b)

let countRange l r =
    [ l .. r + 1 ]
    |> List.map digits
    |> List.filter (fun v -> hasDups v && increasing v)

let groupAdj lst =
    let rec f acc cur =
        function
        | [] -> cur :: acc
        | hd :: tl when hd = (List.head cur) -> f acc (hd :: cur) tl
        | hd :: tl -> f (cur :: acc) [ hd ] tl
    (f [] [ 0 ] lst)
    |> List.rev
    |> List.tail

let partA =
    // Input is "231832-767346"
    printfn "%d" (countRange 231832 767346 |> List.length)

let partB =
    printfn "%O %O" ((digits >> groupAdj) 111233) (digits 111233)
    let res =
        countRange 231832 767346
        |> List.filter (fun v ->
            v
            |> groupAdj
            |> List.exists (fun v -> (List.length v) = 2))
    printfn "%d" (List.length res)
