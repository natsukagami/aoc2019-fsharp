let input =
    System.Console.ReadLine().ToCharArray()
    |> Array.map (fun c -> c.ToString() |> int)
    |> Array.toList

module List =
    let scan f s l =
        let rec r s acc =
            function
            | [] -> acc
            | hd :: tl ->
                let v = f s hd
                r v (v :: acc) tl
        r s [] l
        |> List.rev
        |> List.append [ s ]

    let sums = scan (+) 0 >> Array.ofList

    let repeat n l =
        seq {
            for _ in 1 .. n do
                for j in l do
                    yield j
        }
        |> Seq.toList

let digit x = (abs x) % 10

let pattern (sums: int []) r =
    let rec f acc i sign =
        if i > (List.length input) then acc
        else f (acc + (sums.[min (List.length input) (i + r - 1)] - sums.[i - 1]) * sign) (i + 2 * r) (-sign)
    f 0 r 1 |> digit

let perform skip l =
    let sums = List.sums l
    seq { skip + 1 .. List.length l }
    |> Seq.map (pattern sums)
    |> Seq.append (l |> List.take skip)
    |> Seq.toList


module partA =
    [ 1 .. 10 ]
    |> List.scan (fun l _ -> perform 0 l) input
    |> List.iter (printfn "%A")


    let rec repeat n f =
        match n with
        | 0 -> id
        | _ -> f >> (repeat (n - 1) f)

    printfn "%s"
        (input
         |> repeat 100 (perform 0)
         |> List.map string
         |> fun l -> System.String.Join("", l))

module partB =
    let input = List.repeat 10000 input

    let offset =
        input
        |> List.take 7
        |> List.fold (fun v x -> v * 10 + x) 0

    let cal l = perform ((List.length input) / 2 - 1) l

    // Really slow because of re-creating lists
    let res =
        [ 1 .. 100 ]
        |> Seq.fold (fun l i ->
            printfn "%d" i
            cal l) input

    printfn "%A"
        (res
         |> List.skip offset
         |> List.take 8)
