let input =
    let parseStep (str: string) =
        match str.[0] with
        | 'U' -> (0, str.Substring(1) |> int)
        | 'D' ->
            (0,
             str.Substring(1)
             |> int
             |> (*) -1)
        | 'L' ->
            (str.Substring(1)
             |> int
             |> (*) -1, 0)
        | 'R' -> (str.Substring(1) |> int, 0)
        | _ -> failwith "wtf"

    let add (a, b) (c, d) = (a + c, b + d)

    let parseSteps steps =
        steps
        |> List.map parseStep
        |> List.mapFold (fun s delta -> (add s delta, add s delta)) (0, 0)
        |> fst
        |> fun v -> (0, 0) :: v
        |> List.pairwise

    let read() = System.Console.ReadLine().Split([| ',' |]) |> List.ofArray
    (read() |> parseSteps, read() |> parseSteps)

let cross ((a, b), (c, d)) ((e, f), (g, h)) =
    let sameCross x1 l1 h1 x2 l2 h2 =
        if (x1 = x2) && ((h1 - l1) + (h2 - l2) >= (max h1 h2) - (min l1 l2)) then Some((x1, max l1 l2))
        else None

    let differCross (hx, (hy1, hy2)) ((vx1, vx2), vy) =
        let between x x1 x2 = x1 <= x && x <= x2
        if (between hx vx1 vx2) && (between vy hy1 hy2) then Some((hx, vy))
        else None

    let res =
        if a = c then
            if e = g then sameCross a (min b d) (max b d) e (min f h) (max f h)
            else differCross (a, (min b d, max b d)) (((min e g), (max e g)), f)
        else if f = h then
            sameCross b (min a c) (max a c) f (min e g) (max e g)
        else
            differCross (b, (min a c, max a c)) (((min f h), (max f h)), e)

    printfn "%d %d %d %d %d %d %d %d = %O" a b c d e f g h res
    res

let filterMap fn lst =
    let rec f acc =
        function
        | [] -> acc
        | hd :: tl ->
            f
                (match fn hd with
                 | Some(v) -> v :: acc
                 | None -> acc) tl
    f [] lst |> List.rev

let manhattan (a, b) (c, d) = (abs (a - c)) + (abs (b - d))

let partA =
    let crossLists la lb =
        List.allPairs la lb
        |> filterMap
            ((fun (a, b) ->
            cross a b
            |> Option.map (manhattan (0, 0))
            |> Option.filter (fun v -> v <> 0)))
        |> List.min
    printfn "%d" (crossLists (fst input) (snd input))

let partB =
    let withDist lst =
        lst
        |> List.mapFold (fun s (pa, pb) -> ((s, (pa, pb)), s + (manhattan pa pb))) 0
        |> fst

    let crosses =
        List.allPairs (fst input |> withDist) (snd input |> withDist)
        |> filterMap (fun ((da, pa), (db, pb)) ->
            cross pa pb
            |> Option.filter (fun v -> v <> (0, 0))
            |> Option.map (fun v -> (manhattan (fst pa) v) + (manhattan (fst pb) v) + da + db))

    printfn "%d" (List.min crosses)
