open System.Numerics

type Int = BigInteger

module Intcode =
    let (+.) a (b: int) = Int.op_Addition (a, Int b)

    type Arg =
        | Immediate
        | Position
        | Relative

    type Value =
        { typ: Arg
          value: Int }

    type State =
        { mem: Map<Int, Int>
          rel: Int
          pos: Int
          input: Int list
          output: Int list }

    type Interrupt =
        | Halted of State
        | WantsInput of State
        | Output of State * Int

    module State =
        let create mem (input: int list) =
            let mem =
                mem
                |> Map.toList
                |> List.map (fun (a: int, b) -> (Int a, b))
                |> Map.ofList
            { mem = mem
              rel = Int 0
              pos = Int 0
              input = input |> List.map Int
              output = [] }

        let extract s =
            let { output = output } = s
            match List.tryHead output with
            | Some(v) -> Some({ s with output = output |> List.tail }, v)
            | None -> None

        let push v s = { s with input = v :: s.input }

    module Value =
        let encode typ v =
            { typ = typ
              value = v }

        let apply { typ = typ; value = value } { mem = mem; rel = rel } =
            match typ with
            | Immediate -> value
            | Position ->
                mem
                |> Map.tryFind value
                |> Option.defaultValue (Int 0)
            | Relative ->
                mem
                |> Map.tryFind (value + rel)
                |> Option.defaultValue (Int 0)

        let write target v s =
            let { typ = typ; value = value } = v
            let { mem = mem; rel = rel } = s
            match typ with
            | Immediate -> failwith "Cannot write to an immediate"
            | Position -> { s with mem = mem |> Map.add value target }
            | Relative -> { s with mem = mem |> Map.add (rel + value) target }

    type Op =
        | Exit
        | Add of Value * Value * Value
        | Mul of Value * Value * Value
        | Read of Value
        | Write of Value
        | Jump of (Int -> bool) * Value * Value
        | Compare of (Int -> Int -> bool) * Value * Value * Value
        | ChangeRel of Value

    module Op =
        let parse pos state =
            let bit p n = ((n / bigint.Pow(BigInteger(10), p)) % BigInteger(10)) |> int

            let get p = state |> Map.find p
            let num = state |> Map.find pos

            let toArg p =
                match bit p num with
                | 0 -> Position
                | 1 -> Immediate
                | 2 -> Relative
                | _ -> failwith "unknown arg type"

            let encode offset = Value.encode (toArg (offset + 1)) (get (pos +. offset))
            match num % BigInteger(100) |> int with
            | 99 -> Exit
            | 1 -> Add(encode 1, encode 2, encode 3)
            | 2 -> Mul(encode 1, encode 2, encode 3)
            | 3 -> Read(encode 1)
            | 4 -> Write(encode 1)
            | 5 -> Jump((fun v -> not v.IsZero), encode 1, encode 2)
            | 6 -> Jump((fun v -> v.IsZero), encode 1, encode 2)
            | 7 -> Compare((<), encode 1, encode 2, encode 3)
            | 8 -> Compare((=), encode 1, encode 2, encode 3)
            | 9 -> ChangeRel(encode 1)
            | _ -> failwithf "Unknown opcode %O" num

        let next op pos =
            match op with
            | Exit -> pos +. 1
            | Read _
            | ChangeRel _
            | Write _ -> pos +. 2
            | Jump _ -> pos +. 3
            | Add _
            | Mul _
            | Compare _ -> pos +. 4

        let apply s =
            let { mem = state; pos = pos; input = input; output = output } = s
            let op = parse pos state
            // printfn "%O %O %d" s op (Map.find 15 state)
            let nextState = { s with pos = next op pos }
            match op with
            | Exit -> None
            | Add(a, b, c) -> Some(Value.write ((Value.apply a s) + (Value.apply b s)) c nextState)
            | Mul(a, b, c) -> Some(Value.write ((Value.apply a s) * (Value.apply b s)) c nextState)
            | Read(dest) -> Some { Value.write (List.head input) dest nextState with input = List.tail input }
            | Write(dest) -> Some { nextState with output = (Value.apply dest s) :: output }
            | Jump(cmp, a, b) ->
                let value = Value.apply a s
                let dest = Value.apply b s
                Some
                    { s with
                          pos =
                              (if cmp value then dest
                               else (next op pos)) }
            | Compare(cmp, a, b, c) ->
                let a = Value.apply a s
                let b = Value.apply b s

                let toWrite =
                    if cmp a b then 1
                    else 0
                Some(Value.write (toWrite |> Int) c nextState)
            | ChangeRel(a) -> Some { nextState with rel = s.rel + (Value.apply a s) }

    let rec run state =
        match Op.apply state with
        | None -> state.output
        | Some(v) -> run v

    let rec runUntilOutput state =
        State.extract state
        |> Option.orElseWith (fun () ->
            state
            |> Op.apply
            |> Option.bind runUntilOutput)

    let rec intercept state =
        match Op.parse state.pos state.mem with
        | Read(_) -> WantsInput state
        | Write(v) -> Output(state, Value.apply v state)
        | _ ->
            Op.apply state
            |> Option.map intercept
            |> Option.defaultValue (Halted state)

    let advance state = intercept (Op.apply state |> Option.get)

let initMem =
    System.Console.ReadLine().Split([| ',' |])
    |> Array.map (bigint.Parse)
    |> Array.indexed
    |> Map.ofArray

open Intcode

module List =
    let private join a b = (List.rev a) :: b

    let push item l = List.append l [ item ]

    let splitBy item =

        let rec f acc cur =
            function
            | [] -> join cur acc
            | hd :: tl when hd = item -> f (join cur acc) [] tl
            | hd :: tl -> f acc (hd :: cur) tl
        (f [] []) >> List.rev

    let hasPrefix t =
        let rec f a b =
            match (a, b) with
            | ([], r) -> Some r
            | (a :: ra, b :: rb) when a = b -> f ra rb
            | _ -> None
        f t

    let filterMap fn =
        let rec f acc =
            function
            | [] -> acc
            | hd :: tl ->
                match fn hd with
                | Some(v) -> f (v :: acc) tl
                | _ -> f acc tl
        (f []) >> List.rev

    let splits l =
        let rec f acc cur =
            function
            | [] -> (List.rev cur, []) :: acc
            | hd :: tl -> f ((List.rev cur, hd :: tl) :: acc) (hd :: cur) tl
        f [] [] l |> List.rev

module Space =
    let translate (x: Int) = (int >> char) x

    let map =
        let v = run (State.create initMem []) |> List.rev
        printfn "%A" v
        v
        |> List.map translate
        |> List.splitBy '\n'
        |> List.filter (not << List.isEmpty)
        |> List.map Array.ofList
        |> Array.ofList

module partA =
    //     let map =
    //         let v = "..#..........
    // ..#..........
    // #######...###
    // #.#...#...#.#
    // #############
    // ..#...#...#..
    // ..#####...^.."
    //         v.Split [| '\n' |] |> Array.map (fun s -> s.ToCharArray())
    let map = Space.map

    let dirs =
        [| -1, 0
           0, 1
           1, 0
           0, -1 |]

    let add (x, y) (z, t) = (x + z, y + t)

    let isValid (x, y) = x >= 0 && x < (Array.length map) && y >= 0 && y < (Array.length map.[x])

    let is a (x, y) = isValid (x, y) && (a |> List.contains map.[x].[y])

    let pointer = [ '^'; 'v'; '<'; '>' ]
    let road = '#' :: pointer

    let isIntersection (x, y) =
        ((0, 0) :: (List.ofArray dirs))
        |> List.map (add (x, y))
        |> List.forall (fun d -> is road d)

    let marked =
        map
        |> Array.mapi (fun i v ->
            v
            |> Array.mapi (fun j v ->
                if isIntersection (i, j) then 'O'
                else v))

    let markedSpots =
        marked
        |> Array.mapi (fun i v -> v |> Array.mapi (fun j v -> (i, j, v)))
        |> Array.collect id
        |> Array.filter ((fun (_, _, v) -> v) >> ((=) 'O'))

    let res = markedSpots |> Array.sumBy (fun (i, j, _) -> i * j)

    printfn "%s" (System.String.Join('\n', marked |> Array.map System.String))
    printfn "%A" markedSpots
    printfn "%A" res

module partB =
    open partA

    let isRoad (x, y) = isValid (x, y) && List.contains map.[x].[y] road

    let startPoint =
        map
        |> Array.indexed
        |> Array.pick (fun (i, t) ->
            t
            |> Array.indexed
            |> Array.tryPick (fun (j, v) ->
                if List.contains v pointer then Some(i, j)
                else None))

    let move (cur, dir) = add cur dirs.[dir]

    let moveUpdate (cur, dir) =
        function
        | 0 -> (move (cur, dir), dir)
        | 1 -> (cur, (dir + 1) % 4)
        | -1 -> (cur, (dir + 3) % 4)
        | _ -> failwithf "!"

    let turns =
        let rec f turns state =
            let (cur, _) = state
            if isRoad (move state) then
                f (0 :: turns) (moveUpdate state 0)
            else
                [ -1; 1 ]
                |> List.tryFind (fun v -> isRoad (move (cur, snd (moveUpdate state v))))
                |> Option.map (fun v -> f (v :: turns) (moveUpdate state v))
                |> Option.defaultValue turns
        f [] (startPoint, 0) |> List.rev

    printfn "%A" turns

    let compressCommand l =
        let rec f acc =
            function
            | [] -> acc
            | 0 :: tl ->
                match acc with
                | (0, c) :: r -> f ((0, c + 1) :: r) tl
                | t -> f ((0, 1) :: t) tl
            | hd :: tl -> f ((1, hd) :: acc) tl
        f [] l |> List.rev

    let (main, commands) =
        let better (a, b) =
            function
            | Some(c, d) ->
                let cost a b = max (List.length a) (List.map (snd >> List.length) b |> List.max)
                Some
                    (if cost a b < cost c d then (a, b)
                     else (c, d))
            | None -> Some(a, b)

        let rec f best acc commands =
            function
            | [] -> better (acc, commands) best
            | xs ->
                // try each command
                let withCommand =
                    commands
                    |> List.filterMap (fun (v, _) -> List.hasPrefix v xs |> Option.map (fun t -> (v, t)))
                    |> List.fold (fun best (v, t) -> f best (v :: acc) commands t) best
                // with new commands
                match commands with
                | [ _; _; _ ] -> withCommand
                | _ ->
                    List.splits xs
                    |> List.tail
                    |> List.map (fun (pref, suf) -> ((pref, compressCommand pref), suf))
                    |> List.takeWhile
                        (fst
                         >> snd
                         >> List.length
                         >> (>=) 10)
                    |> List.fold (fun best ((pref, cPref), suf) -> f best (pref :: acc) ((pref, cPref) :: commands) suf)
                           withCommand

        f None [] [] turns
        |> Option.get
        |> fun (a, b) -> (List.rev a, b)

    let compressMain commands =
        let t =
            commands
            |> List.map fst
            |> List.indexed
        List.map (fun v ->
            t
            |> List.pick (fun (i, x) ->
                if v = x then Some(char (i + (int 'A')))
                else None))

    let compressedMain =
        compressMain commands main
        |> Array.ofList
        |> fun v -> System.String.Join(',', v).ToCharArray()
        |> Array.map (int)
        |> Array.toList
        |> List.push (int '\n')

    let compressedCommands =
        let writeCommand l =
            l
            |> List.map (function
                | (0, v) -> (string v)
                | (1, 1) -> "R"
                | (1, -1) -> "L"
                | _ -> failwith "!!")
            |> List.toArray
            |> fun v -> System.String.Join(',', v).ToCharArray()
            |> Array.map (int)
        commands
        |> List.map (snd >> writeCommand)
        |> List.collect (Array.toList >> List.push (int '\n'))

    printfn "%A %A" compressedMain compressedCommands

    let state =
        (State.create (initMem |> Map.add 0 (Int 2))
             (List.append (List.append compressedMain compressedCommands)
                  [ (int 'n')
                    (int '\n') ]))

    printfn "%A" (run state)
