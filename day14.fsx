type Compound =
    { count: int64
      chem: string }

type Reaction = Compound * Compound list

module Compound =
    let parse (s: string) =
        match s.Trim().Split(' ') with
        | [| count; chem |] ->
            { count = int64 count
              chem = chem }
        | _ -> failwithf "Invalid compound %s" s

let input: Reaction list =
    let read _ = System.Console.ReadLine()
    Seq.initInfinite read
    |> Seq.takeWhile (not << isNull)
    |> Seq.map
        ((fun s -> s.Split("=>"))
         >> (function
         | [| ingredients; target |] ->
             (Compound.parse target,
              ingredients.Split ','
              |> Array.map Compound.parse
              |> Array.toList)
         | t -> failwithf "Invalid reaction %A" t))
    |> Seq.toList

let reactionMap =
    input
    |> Seq.map (fun v -> ((fst v).chem, v))
    |> Map.ofSeq

let find0 item = Map.tryFind item >> Option.defaultValue 0L

module partA =
    let rec run sum leftovers =
        function
        | [] -> sum
        | { count = count; chem = "ORE" } :: tl -> run (count + sum) leftovers tl
        | { count = count; chem = chem } :: tl ->
            match leftovers |> find0 chem with
            | 0L ->
                let (target, ins) = reactionMap |> Map.find chem
                let times = (count + target.count - 1L) / target.count
                let leftover = target.count * times - count
                run sum (leftovers |> Map.add chem ((leftovers |> find0 chem) + leftover))
                    (List.append tl
                         (ins
                          |> List.map (fun { count = c; chem = ch } ->
                              { count = c * times
                                chem = ch })))
            | v when v >= count -> run sum (leftovers |> Map.add chem (v - count)) tl
            | v ->
                run sum (leftovers |> Map.remove chem)
                    ({ count = count - v
                       chem = chem }
                     :: tl)

    let res =
        run 0L Map.empty
            [ { count = 1L
                chem = "FUEL" } ]

    printfn "%d" res

module partB =
    let resFor n =
        partA.run 0L Map.empty
            [ { count = n
                chem = "FUEL" } ]

    let target = 1000000000000L

    let rec binarySearch min max =
        if min = max then
            min - 1L
        else
            let mid = (min + max) / 2L
            if resFor mid <= target then binarySearch (mid + 1L) max
            else binarySearch min mid

    let res = binarySearch 1L 1000000000L

    printfn "%d %d" res (resFor res)
