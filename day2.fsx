let input =
    System.Console.ReadLine().Split([| ',' |])
    |> Array.map int
    |> Array.indexed
    |> Map.ofArray

let load input a b =
    input
    |> Map.add 1 a
    |> Map.add 2 b

let deref idx mp = Map.find (Map.find idx mp) mp

let rec run input step =
    match input |> Map.find step with
    | 99 -> input |> Map.find 0
    | 1 ->
        run (input |> Map.add (input |> Map.find (step + 3)) (deref (step + 1) input + deref (step + 2) input))
            (step + 4)
    | 2 ->
        run (input |> Map.add (input |> Map.find (step + 3)) (deref (step + 1) input * deref (step + 2) input))
            (step + 4)
    | _ -> failwith "boom"

let partA() = printfn "%d" (run (load input 12 2) 0)

let partB() =
    let res =
        Seq.allPairs [ 0 .. 100 ] [ 0 .. 100 ]
        |> Seq.find (fun (a, b) -> run (load input a b) 0 = 19690720)
        |> fun (a, b) -> 100 * a + b
    printfn "%d" res

partB()
