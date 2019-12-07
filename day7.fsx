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

type State =
    { mem: Map<int, int>
      pos: int
      input: int list
      output: int list }

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
        | 3 -> Read(get (pos + 1))
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

    let apply s =
        let { mem = state; pos = pos; input = input; output = output } = s
        let op = parse pos state
        // printfn "%O %O %d" s op (Map.find 15 state)
        let nextState = { s with pos = next op pos }
        match op with
        | Exit -> None
        | Add(a, b, c) ->
            Some { nextState with mem = state |> Map.add c ((Value.apply a state) + (Value.apply b state)) }
        | Mul(a, b, c) ->
            Some { nextState with mem = state |> Map.add c ((Value.apply a state) * (Value.apply b state)) }
        | Read(dest) ->
            Some
                { nextState with
                      mem = state |> Map.add dest (List.head input)
                      input = List.tail input }
        | Write(dest) -> Some { nextState with output = (Value.apply dest state) :: output }
        | Jump(cmp, a, b) ->
            let value = Value.apply a state
            let dest = Value.apply b state
            Some
                { s with
                      pos =
                          (if cmp value then dest
                           else (next op pos)) }
        | Compare(cmp, a, b, c) ->
            let a = Value.apply a state
            let b = Value.apply b state

            let toWrite =
                if cmp a b then 1
                else 0
            Some { nextState with mem = state |> Map.add c toWrite }

let rec run state =
    match Op.apply state with
    | None -> state.output
    | Some(v) -> run v

let initialState =
    System.Console.ReadLine().Split([| ',' |])
    |> Array.map int
    |> Array.indexed
    |> Map.ofArray

let inputs set =
    let rec f cur =
        function
        | [] -> [ cur ]
        | v -> v |> List.collect (fun t -> f (t :: cur) (v |> List.filter ((<>) t)))
    f [] set

let partA() =

    let tryInput input =
        let res =
            run
                { mem = initialState
                  pos = 0
                  input = input
                  output = [] }
        // printfn "%O => %O" input res
        List.head res

    let trySeq s =
        let rec f lastSig =
            function
            | [] -> lastSig
            | hd :: tl -> f (tryInput [ hd; lastSig ]) tl
        f 0 s

    let res =
        inputs [ 0 .. 4 ]
        |> List.map trySeq
        |> List.max

    printfn "%d" res

type Continuation =
    | Transmit of State * int
    | Stopped of State

let partB =
    let rec customRun state =
        let { mem = mem; pos = pos; input = input; output = output } = state
        let op = Op.parse pos mem
        match op with
        | Write(dest) ->
            let v = Value.apply dest mem
            Transmit({ state with pos = Op.next op pos }, v)
        | _ ->
            match Op.apply state with
            | Some(v) -> customRun v
            | None -> Stopped(state)

    let tryInput input =
        let machines =
            input
            |> List.mapi (fun i v ->
                { mem = initialState
                  pos = 0
                  input =
                      (if i = 0 then [ v; 0 ]
                       else [ v ])
                  output = [] })
            |> List.indexed
            |> Map.ofList

        let rec run index machines =
            match customRun (Map.find index machines) with
            | Transmit(state, value) ->
                let next =
                    (if index = 4 then 0
                     else index + 1)

                let oldNextMachine = Map.find next machines

                let newMachines =
                    machines
                    |> Map.add next { oldNextMachine with input = List.append oldNextMachine.input [ value ] }
                    |> Map.add index state
                run next newMachines
            | Stopped(state) -> state

        let v = run 0 machines
        List.head v.input

    let res =
        inputs [ 5 .. 9 ]
        |> List.map ((fun v -> List.append v [ 0 ]) >> tryInput)
        |> List.max

    printfn "%d" res
