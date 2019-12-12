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

type Colors = Map<Int * Int, Int>

module Colors =
    let get d (x, y) (c: Colors) =
        c
        |> Map.tryFind (x, y)
        |> Option.defaultValue d

    let set (x, y) z (c: Colors): Colors = c |> Map.add (x, y) z

    let count (c: Colors) =
        c
        |> Map.toList
        |> List.length

    let directionMatrix =
        [ (0, 1)
          (1, 0)
          (0, -1)
          (-1, 0) ]
        |> List.map (fun (a, b) -> (Int a, Int b))
        |> Array.ofList

    let apply (x, y) (dir: Int) =
        let (z, t) = directionMatrix.[dir |> int]
        (x + z, y + t)

    let change diff (dir: Int) =
        (dir + (if diff = Int.Zero then Int 3
                else Int 1)) % (Int 4)

    let startingDir = Int 0

let initMem =
    System.Console.ReadLine().Split([| ',' |])
    |> Array.map (bigint.Parse)
    |> Array.indexed
    |> Map.ofArray

open Intcode

let run d state =
    let rec f colors pos dir state =
        printfn "%A %A" pos dir
        let current = Colors.get d pos colors
        match runUntilOutput (State.push current state) with
        | None -> colors
        | Some(state, v) ->
            let colors = Colors.set pos v colors
            let (state, v) = runUntilOutput state |> Option.get
            let dir = Colors.change v dir
            f colors (Colors.apply pos dir) dir state
    f Map.empty (Int 0, Int 0) Colors.startingDir state

let partA() =
    let res = run (Int 0) (State.create initMem [])
    printfn "%d" (res |> Colors.count)

let partB =
    let res = run (Int 1) (State.create initMem [])

    let Xs =
        res
        |> Map.toSeq
        |> Seq.map (fun ((a, _), _) -> a)

    let Ys =
        res
        |> Map.toSeq
        |> Seq.map (fun ((_, b), _) -> b)

    seq { Ys |> Seq.min .. Ys |> Seq.max }
    |> Seq.rev
    |> Seq.map (fun x ->
        seq { Xs |> Seq.min .. Xs |> Seq.max }
        |> Seq.map
            ((fun y -> (y, x))
             >> (fun v ->
             Map.tryFind v res
             |> function
             | Some(v) when v = Int 1 -> '#'
             | None -> '#'
             | _ -> ' '))
        |> Seq.toArray
        |> System.String)
    |> Seq.iter (printfn "%s")
