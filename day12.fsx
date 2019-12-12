type Val =
    { x: int
      y: int
      z: int }

module Val =
    let add { x = x1; y = y1; z = z1 } { x = x2; y = y2; z = z2 } =
        { x = x1 + x2
          y = y1 + y2
          z = z1 + z2 }

    let abs { x = x; y = y; z = z } = abs x + abs y + abs z

type Moon =
    { pos: Val
      vel: Val }

module Moon =
    let energy { pos = pos; vel = vel } = Val.abs pos * Val.abs vel

    let updatePos { pos = pos; vel = vel } =
        { pos = Val.add pos vel
          vel = vel }

    let updateVel lst (m: Moon) =
        let compare x y =
            if x < y then 1
            else if x = y then 0
            else -1

        let compareVal { x = x1; y = y1; z = z1 } { x = x2; y = y2; z = z2 } =
            { x = compare x1 x2
              y = compare y1 y2
              z = compare z1 z2 }

        { m with
              vel =
                  lst
                  |> List.map ((fun v -> v.pos) >> (compareVal m.pos))
                  |> List.fold Val.add (m.vel) }

    let update lst = List.map (updateVel lst >> updatePos) lst

module Parse =
    let moon (s: string) =
        s
        |> (fun s -> s.Substring(1, s.Length - 2)) // Remove < and >
        |> (fun s -> s.Split([| ',' |]))
        |> Array.map (fun s ->
            s.Trim().Split([| '=' |])
            |> function
            | [| _; v |] -> int v
            | _ -> failwith "bleh")
        |> function
        | [| x; y; z |] ->
            { pos =
                  { x = x
                    y = y
                    z = z }
              vel =
                  { x = 0
                    y = 0
                    z = 0 } }
        | _ -> failwith "bleh!"

let input =
    let read _ = System.Console.ReadLine()
    Seq.initInfinite read
    |> Seq.takeWhile (isNull >> not)
    |> Seq.map Parse.moon
    |> Seq.toList

let partA =
    let res =
        seq { 1 .. 1000 }
        |> Seq.fold (fun s _ -> Moon.update s) input
        |> Seq.sumBy Moon.energy
    printfn "%d" res

type Moon1D =
    { pos: int
      vel: int }

module Moon1D =
    let updatePos { Moon1D.pos = pos; vel = vel } =
        { pos = pos + vel
          vel = vel }

    let updateVel lst m =
        let newVel =
            m.vel + (lst
                     |> List.sumBy (fun v ->
                         if v.pos > m.pos then 1
                         else if v.pos = m.pos then 0
                         else -1))
        { m with vel = newVel }

    let update lst = List.map (updateVel lst >> updatePos) lst

    let stepsToRotate lst =
        let rec f acc l =
            if l = lst then acc
            else f (acc + 1L) (update l)
        f 1L (update lst)

let partB =
    let a =
        Moon1D.stepsToRotate
            (input
             |> List.map (fun m ->
                 { pos = m.pos.x
                   vel = m.vel.x }))

    let b =
        Moon1D.stepsToRotate
            (input
             |> List.map (fun m ->
                 { pos = m.pos.y
                   vel = m.vel.y }))

    let c =
        Moon1D.stepsToRotate
            (input
             |> List.map (fun m ->
                 { pos = m.pos.z
                   vel = m.vel.z }))

    let rec gcd a =
        function
        | 0L -> a
        | b -> gcd b (a % b)

    let lcm a b = a / (gcd a b) * b

    printfn "%d %d %d %d" a b c (lcm (lcm a b) c)
