open System
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators.OperatorIntrinsics

let part1 input iterations = 
    let buffer = new List<int>()
    buffer.Add(0)
    let mutable position = 0

    for value = 1 to iterations do
        position <- ((position + input) % buffer.Count)
        buffer.Insert(position + 1, value)
        position <- position + 1
        
    buffer.Item((position + 1) % buffer.Count)

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

let rec part2Recursive input (iteration, remaining, pos, az) = 
    match remaining with
    | 0 -> az
    | _ ->
        let position = ((pos + input) % iteration)
        let afterZero = (if position = 0 then iteration else az)
        part2Recursive input ((iteration + 1), (remaining - 1), position + 1, afterZero)

[<EntryPoint>]
let main argv =

    let input = 301
    printfn "solution 1: %i" <| part1 input 2017
    printfn "solution 2: %i" <| part2 input 50000000
    printfn "solution 2_proper: %i" <| part2Recursive input (1, 50000000, 0, 0)
    Console.ReadLine() |> ignore

    0
