let width = 25
let height = 6

let input =
    let s = System.Console.ReadLine()
    s.ToCharArray()
    |> Array.splitInto ((String.length s) / (width * height))
    |> List.ofArray

let count cond arr =
    let rec f acc =
        function
        | n when n = Array.length arr -> acc
        | n ->
            f
                ((if cond arr.[n] then 1
                  else 0)
                 + acc) (n + 1)
    f 0 0

let combine a b =
    match a with
    | '2' -> b
    | _ -> a

let partA =
    let layer = input |> List.minBy (count ((=) '0'))
    let res = (layer |> count ((=) '1')) * (layer |> count ((=) '2'))
    printfn "%d" res

let partB =
    let combineArray = Array.map2 combine
    let blank = Array.init (width * height) (fun _ -> '2')
    let res = List.fold combineArray blank input
    printfn "%s"
        (res
         |> Array.map (function
             | '0' -> ' '
             | a -> a)
         |> Array.splitInto height
         |> Array.map System.String
         |> fun v -> System.String.Join('\n', v))
