let edges =
    let read _ = System.Console.ReadLine()
    Seq.initInfinite read
    |> Seq.takeWhile (function
        | null -> false
        | _ -> true)
    |> Seq.map (fun v -> v.Split([| ')' |]) |> fun v -> (v.[0], v.[1]))
    |> Seq.toList

let addEdge graph (a, b) = graph |> Map.add a (b :: (Map.tryFind a graph |> Option.defaultValue []))

let graph = edges |> List.fold addEdge Map.empty

let nodes = List.append (edges |> List.map fst) (edges |> List.map snd) |> List.distinct

let memoize<'a, 'b when 'a: equality> (fn: ('a -> 'b) -> 'a -> 'b) =
    let mp = new System.Collections.Generic.Dictionary<'a, 'b>()

    let rec cal v =
        match mp.TryGetValue v with
        | true, res -> res
        | false, _ ->
            let res = fn cal v
            mp.Add(v, res)
            res
    cal

let partA =
    let f =
        let f f' s =
            Map.tryFind s graph
            |> Option.defaultValue []
            |> List.sumBy (fun v -> (f' v) + 1)
        memoize f


    let res = nodes |> List.sumBy f
    printfn "%d" res

let partB =
    let graph =
        // Make it undirected
        edges
        |> List.map (fun (a, b) -> (b, a))
        |> List.fold addEdge graph
    let rec bfs queue distances =
        match queue with
        | [] -> distances
        | hd :: tl ->
            let toUpdate =
                graph
                |> Map.tryFind hd
                |> Option.defaultValue []
                |> List.filter (fun v -> not (Map.containsKey v distances))

            let currentDist = Map.find hd distances
            let updatedDists = toUpdate |> List.fold (fun mp v -> Map.add v (currentDist + 1) mp) distances
            let newQueue = List.append toUpdate tl
            bfs newQueue updatedDists

    let fromYou = bfs [ "YOU" ] (Map.ofList [ ("YOU", 0) ])
    printfn "%d" ((Map.find "SAN" fromYou) - 2)
