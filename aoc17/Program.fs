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

[<EntryPoint>]
let main argv =

    let input = 301
    printfn "solution 1: %i" <| part1 input 2017
    printfn "solution 2: %i" <| part2 input 50000000

    Console.ReadLine() |> ignore

    0
