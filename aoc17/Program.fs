open System
open System.Collections.Generic

let part1 input iterations = 
    let buffer = new List<int>()
    buffer.Add(0)
    let mutable position = 0

    for value = 1 to iterations do
        position <- ((position + input) % buffer.Count)
        buffer.Insert(position + 1, value)
        position <- position + 1
        
    buffer.Item((position + 1) % buffer.Count)

let rec part1Recursive input (iteration, remaining, position, buffer:List<int>) =
    match remaining with
    | 0 -> buffer.Item((position + 1) % buffer.Count)
    | _ ->
        let pos = ((position + input) % buffer.Count) + 1
        buffer.Insert(pos, iteration)
        part1Recursive input (iteration + 1, remaining - 1, pos, buffer)

let rec part1Immutable input (iteration, remaining, position, buffer:list<int>) = 
    match remaining with 
    | 0 -> buffer.[((position + 1) % buffer.Length)]
    | _ ->
            let pos = ((position + input) % buffer.Length)
            let buf = buffer.[0..pos] @ iteration :: if pos = buffer.Length - 1 then [] else buffer.[pos+1..]
            part1Immutable input (iteration + 1, remaining - 1, pos + 1, buf)

let part2 input iterations = 
    let mutable position = 0
    let mutable count = 1
    let mutable afterZero = 0

    for value = 1 to iterations do
        position <- ((position + input) % count)
        if position = 0 then afterZero <- value
        position <- position + 1
        count <- count + 1

    afterZero

let rec part2Recursive input (iteration, remaining, position, afterZero) = 
    match remaining with
    | 0 -> afterZero
    | _ ->
        let pos = ((position + input) % iteration)
        let az = (if pos = 0 then iteration else afterZero)
        part2Recursive input ((iteration + 1), (remaining - 1), pos + 1, az)

[<EntryPoint>]
let main argv =

    let input = 301
    printfn "solution 1: %i" <| part1 input 2017

    let buffer = new List<int>()
    buffer.Add(0)
    printfn "solution 1 recursive: %i" <| part1Recursive input (1, 2017, 0, buffer)
    printfn "solution 1 immutable: %i" <| part1Immutable input (1, 2017, 0, [0;])

    printfn "solution 2: %i" <| part2 input 50000000
    printfn "solution 2 recursive: %i" <| part2Recursive input (1, 50000000, 0, 0)
    Console.ReadLine() |> ignore

    0
