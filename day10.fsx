let input =
    let read _ = System.Console.ReadLine()
    Seq.initInfinite read
    |> Seq.takeWhile (isNull >> not)
    |> Seq.map (fun v -> v.ToCharArray() |> Array.map ((=) '#'))
    |> Seq.toArray

type Point =
    { x: int
      y: int }

type Line =
    | Vertical of bool
    | Horizontal of bool
    | Diagonal of int * int

module Line =
    let quad =
        function
        | Vertical v ->
            (if v then 2
             else 0)
        | Horizontal v ->
            if v then 1
            else 3
        | Diagonal(x, y) when x < 0 ->
            if y < 0 then 3
            else 0
        | Diagonal(_, y) ->
            if y < 0 then 2
            else 1

    let compare a b =
        if (quad a) = (quad b) then
            match (a, b) with
            | Diagonal(x1, y1), Diagonal(x2, y2) -> x1 * y2 - y1 * x2 // CCW formula
            | _, Diagonal _ -> -1
            | Diagonal _, _ -> 1
            | _, _ -> 0
        else
            (quad a) - (quad b)

module Point =
    let (+) { x = x1; y = y1 } { x = x2; y = y2 } =
        { x = x1 + x2
          y = y1 + y2 }

    let neg { x = x; y = y } =
        { x = -x
          y = -y }

    let rec gcd a =
        function
        | 0 -> a
        | b -> gcd b (a % b)

    let line from until: Line =
        let { x = x1; y = y1 } = from
        let { x = x2; y = y2 } = until
        if x1 = x2 then
            Horizontal(y2 > y1)
        else if y1 = y2 then
            Vertical(x2 > x1)
        else
            let x = x2 - x1
            let y = y2 - y1
            let d = gcd (abs x) (abs y)
            Diagonal(x / d, y / d)

let n = input |> Array.length
let m = input.[0] |> Array.length

let inBoard { x = x; y = y } = x >= 0 && x < n && y >= 0 && y < m

let points =
    [ 0 .. n - 1 ]
    |> List.allPairs [ 0 .. m - 1 ]
    |> List.map (fun (a, b) ->
        { x = a
          y = b })

let pointDeltas =
    points
    |> List.tail // Remove 0, 0
    |> List.collect (fun v ->
        [ v
          { v with y = -v.y }
          { v with x = -v.x }
          { x = -v.x
            y = -v.y } ])
    |> List.distinct

let cal point =
    pointDeltas
    |> List.toSeq
    |> Seq.map (Point.op_Addition point)
    |> Seq.filter inBoard
    |> Seq.filter (fun v -> input.[v.x].[v.y])
    |> Seq.map (fun v -> (Point.line point v), Point.op_Addition v (Point.neg point))

let partA =
    let getValue =
        Seq.distinctBy fst
        >> Seq.toList
        >> List.length

    let res =
        points
        |> List.filter (fun v -> input.[v.x].[v.y])
        |> List.map (fun v -> (cal v, v))
        |> List.maxBy (fst >> getValue)

    printfn "%A" res
    res |> snd

let partB =
    let targets = cal partA

    let angles =
        targets
        |> Seq.map fst
        |> Seq.distinct
        |> Seq.sortWith Line.compare

    printfn "%A" angles

    let targetSet =
        targets
        |> Seq.groupBy fst
        |> Seq.map (fun (a, b) ->
            (a,
             b
             |> Seq.map snd
             |> Seq.toList))
        |> Map.ofSeq
        |> Map.map (fun _ v -> v |> List.sortBy (fun p -> abs (p.x)))

    let res =
        let toScan =
            seq {
                while true do
                    yield! angles
            }

        let scanTarget =
            seq {
                let mutable targets = targetSet
                let toScan = toScan |> Seq.takeWhile (fun _ -> not (Map.isEmpty targets))
                for i in toScan do
                    match targets |> Map.tryFind i with
                    | Some(hd :: tl) ->
                        yield (Point.op_Addition hd partA)
                        if Seq.isEmpty tl then targets <- targets |> Map.remove i
                        else targets <- targets |> Map.add i tl
                    | _ -> ()
            }

        scanTarget |> Seq.skip 199


    printfn "%A" res
