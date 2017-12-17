// Learn more about F# at http://fsharp.org

open System
open System.Collections.Generic

[<EntryPoint>]
let main argv =

    let buffer = new List<int>()
    buffer.Add(0)
    let mutable position = 0

    let input = 301

    for value = 1 to 2017 do
        position <- ((position + input) % buffer.Count)
        //buffer.Insert((position + 1) % (buffer.Count - 1), value)
        buffer.Insert(position + 1, value)
        position <- position + 1
        //printfn "position: %i" position

    //buffer |> Seq.iter (fun d -> printf "%i," d)

    printfn "solution 1: %i" <| buffer.Item(position + 1)

    Console.ReadLine()

    0 // return an integer exit code
