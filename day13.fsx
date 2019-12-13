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

let getTile state =
    let rec f acc state =
        match acc with
        | [ c; b; a ] -> Some(state, (a, b), c)
        | _ -> runUntilOutput state |> Option.bind (fun (state, v) -> f (v :: acc) state)
    f [] state

module partA =
    let getTiles state =
        let rec f acc state =
            getTile state
            |> Option.map (fun (state, coord, color) -> f ((coord, color) :: acc) state)
            |> Option.defaultValue acc
        f [] state

    let res =
        getTiles (State.create initMem [])
        |> Seq.filter (fun (_, c) -> c = Int 2)
        |> Seq.length

    printfn "%d" res

type Game =
    { map: Map<Int * Int, Int>
      score: Int }

module Game =
    let empty =
        { map = Map.empty
          score = Int 0 }

    let update ((x, y), v) g =
        let { map = map; score = score } = g
        if (x = Int -1) && (y = Int 0) then { g with score = v }
        else { g with map = map |> Map.add (x, y) v }

    let create = List.fold (fun g v -> update v g) empty

    let ball game = Map.findKey (fun _ v -> v = Int 4) game.map
    let plate game = Map.findKey (fun _ v -> v = Int 3) game.map

    let print s =
        System.Console.Clear()
        let { map = map; score = score } = s

        let drawChar (v: Int) =
            match (int v) with
            | 1 -> 'W'
            | 2 -> '#'
            | 3 -> '_'
            | 4 -> '*'
            | _ -> ' '
        let keys =
            map
            |> Map.toList
            |> List.map fst

        let xs = keys |> List.map fst
        let ys = keys |> List.map snd
        let xs = [ xs |> List.min .. xs |> List.max ]
        let ball = ball s
        let plate = plate s
        seq { ys |> List.min .. ys |> List.max }
        |> Seq.map (fun y ->
            xs
            |> List.map (fun x ->
                Map.tryFind (x, y) map
                |> Option.defaultValue (Int 0)
                |> drawChar)
            |> List.toArray
            |> System.String)
        |> Seq.iter (printfn "%s")
        printfn "Score = %A" score
        printfn "Ball = %A" ball
        printfn "Plate = %A" plate

module partB =
    let interact state =
        let rec f game acc intr =
            match intr with
            | Halted _ -> game.score
            | WantsInput state ->
                Game.print game
                let input =
                    let ball = Game.ball game |> fst
                    let plate = Game.plate game |> fst
                    (if plate < ball then 1
                     else if plate = ball then 0
                     else -1)
                    |> Int
                printfn "Please input: %A" input
                f game acc (State.push input state |> advance)
            | Output(state, o) ->
                match o :: acc with
                | [ c; b; a ] ->
                    let game = Game.update ((a, b), c) game
                    f game [] (advance state)
                | acc -> f game acc (advance state)
        f Game.empty [] (intercept state)

    let state =
        let s = (State.create initMem [])
        { s with mem = s.mem |> Map.add (Int 0) (Int 2) }

    let res = interact state

    printfn "%A" res
