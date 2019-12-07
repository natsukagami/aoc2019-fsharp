let input =
    let read() =
        match System.Console.ReadLine() with
        | null -> None
        | v -> Some(v)
    Seq.unfold (fun () -> read() |> Option.map (fun v -> (v, ()))) () |> Seq.toList

let calculate n = (n / 3) - 2

let partA =
    let sum = input |> Seq.sumBy (int >> calculate)
    printfn "%d" sum

let partB =
    let cal n = max 0 (calculate n)

    let rec calRec acc =
        function
        | 0 -> acc
        | n -> calRec (acc + (cal n)) (cal n)

    let sum = input |> Seq.sumBy (int >> calRec 0)
    printfn "%d" sum
 