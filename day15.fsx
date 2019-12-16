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

module Space =
    let add (a, b) (c, d) = (a + c, b + d)

    let dir x =
        let arr =
            [| (0, 0)
               (0, 1)
               (0, -1)
               (-1, 0)
               (1, 0) |]
        arr.[x]

    let private send (dir: int) state =
        let state =
            match intercept state with
            | WantsInput s -> (State.push (Int dir) s)
            | t -> failwithf "Got intercept %A" t
        match advance state with
        | Output(s, v) -> (Op.apply s |> Option.get, int v)
        | t -> failwithf "Got intercept %A" t

    let rec private bfs outputs dists =
        function
        | [] -> (outputs, dists)
        | (state, coord) :: tl ->
            let v =
                seq { 1 .. 4 }
                |> Seq.map (fun v -> (send v state, add coord (dir v)))
                |> Seq.filter (fun ((_, t), c) -> t > 0 && (not (Map.containsKey c dists)))
                |> Seq.toList

            let cur = (dists |> Map.find coord)
            bfs (List.fold (fun d (o, c) -> Map.add c o d) outputs v)
                (List.fold (fun d (_, c) -> Map.add c (1 + cur) d) dists v)
                (List.append tl (List.map (fun ((s, _), c) -> s, c) v))

    let bfsFrom state coord =
        let f = Map.ofList
        bfs (f [ (coord, (state, 1)) ]) (f [ (coord, 0) ]) [ (state, coord) ]

module partA =
    let initState = State.create initMem []
    let (outputs, dists) = Space.bfsFrom initState (0, 0)

    let target =
        outputs
        |> Map.toList
        |> List.pick (fun (c, (s, t)) ->
            match t with
            | 2 -> Some(c, s)
            | _ -> None)

    printfn "%A" (dists |> Map.find (fst target))

module partB =
    let (coord, state) = partA.target
    let (_, dists) = Space.bfsFrom state coord

    printfn "%A"
        (dists
         |> Map.toSeq
         |> Seq.map snd
         |> Seq.max)
