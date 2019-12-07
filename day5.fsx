type Arg =
    | Immediate
    | Position

type Value =
    { typ: Arg
      value: int }

module Value =
    let encode typ v =
        { typ = typ
          value = v }

    let apply { typ = typ; value = value } state =
        match typ with
        | Immediate -> value
        | Position -> state |> Map.find value

type Op =
    | Exit
    | Add of Value * Value * int
    | Mul of Value * Value * int
    | Read of int
    | Write of Value
    | Jump of (int -> bool) * Value * Value
    | Compare of (int -> int -> bool) * Value * Value * int

module Op =
    let parse pos state =
        let rec bit p n =
            if p = 0 then n % 10
            else bit (p - 1) (n / 10)

        let get p = state |> Map.find p
        let num = state |> Map.find pos

        let toArg p =
            if (bit p num) = 0 then Position
            else Immediate
        match num % 100 with
        | 99 -> Exit
        | 1 -> Add(Value.encode (toArg 2) (get (pos + 1)), Value.encode (toArg 3) (get (pos + 2)), (get (pos + 3)))
        | 2 -> Mul(Value.encode (toArg 2) (get (pos + 1)), Value.encode (toArg 3) (get (pos + 2)), (get (pos + 3)))
        | 3 -> Read(get (pos + 3))
        | 4 -> Write(Value.encode (toArg 2) (get (pos + 1)))
        | 5 -> Jump((fun v -> v <> 0), Value.encode (toArg 2) (get (pos + 1)), Value.encode (toArg 3) (get (pos + 2)))
        | 6 -> Jump((fun v -> v = 0), Value.encode (toArg 2) (get (pos + 1)), Value.encode (toArg 3) (get (pos + 2)))
        | 7 ->
            Compare
                ((<), Value.encode (toArg 2) (get (pos + 1)), Value.encode (toArg 3) (get (pos + 2)), (get (pos + 3)))
        | 8 ->
            Compare
                ((=), Value.encode (toArg 2) (get (pos + 1)), Value.encode (toArg 3) (get (pos + 2)), (get (pos + 3)))
        | _ -> failwithf "Unknown opcode %d" num

    let next op pos =
        match op with
        | Exit -> pos + 1
        | Read _
        | Write _ -> pos + 2
        | Jump _ -> pos + 3
        | Add _
        | Mul _
        | Compare _ -> pos + 4

    let apply (state, input, pos) =
        let op = parse pos state
        match op with
        | Exit -> None
        | Add(a, b, c) -> Some(state |> Map.add c ((Value.apply a state) + (Value.apply b state)), input, next op pos)
        | Mul(a, b, c) -> Some(state |> Map.add c ((Value.apply a state) * (Value.apply b state)), input, next op pos)
        | Read(dest) -> Some(state |> Map.add dest (List.head input), List.tail input, next op pos)
        | Write(dest) -> Some(state, input, next op pos)
        | Jump(cmp, a, b) ->
            let value = Value.apply a state
            let dest = Value.apply b state
            Some
                (state, input,
                 (if cmp value then dest
                  else (next op pos)))
        | Compare(cmp, a, b, c) ->
            let a = Value.apply a state
            let b = Value.apply b state

            let toWrite =
                if cmp a b then 1
                else 0
            Some(state |> Map.add c toWrite, input, next op pos)

let initialState =
    System.Console.ReadLine().Split([| ',' |])
    |> Array.map int
    |> Array.indexed
    |> Map.ofArray


let run (state, input) =
    let rec f (state, input, pos) =
        match Op.apply (state, input, pos) with
        | None -> ()
        | Some(v) -> f v
    f (state, input, 0)

// let partA =
//     let initialInput = [ 1 ]
//     run (initialState, initialInput)

let partB =
    let initialInput = [ 5 ]
    run (initialState, initialInput)
