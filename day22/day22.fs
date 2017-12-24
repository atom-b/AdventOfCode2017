// Learn more about F# at http://fsharp.org

open System
open System.Numerics

let infectedState = "#"
let cleanState = "."
let weakenedState = "W"
let flaggedState = "F"
let readLines filePath = System.IO.File.ReadLines(filePath)

let inputToGrid (input:string[]) =
        input |> Array.map (fun row -> row.ToCharArray() |> Array.map (fun c -> c.ToString()))

let gridToDict (g:string[][]) =
    g
    |> Array.mapi (fun y row -> row |> Array.mapi (fun x cell -> ((x,y),cell) ))
    |> Array.reduce (fun acc row -> Array.concat [acc; row] )
    |> Map.ofArray

let right dir =
    let dir_x, dir_y = dir
    if dir_x = 1 then (0, -1)
    else if dir_x = -1 then (0,1)
    else if dir_y = 1 then (1,0)
    else (-1,0)

let left dir =
    let dir_x, dir_y = dir
    if dir_x = 1 then (0, 1)
    else if dir_x = -1 then (0,-1)
    else if dir_y = 1 then (-1,0)
    else (1,0)

let updateNodePart1 (node:((int*int)*string)) =
    let pos,state = node
    let newState =
        match state with
        | s when s = cleanState -> infectedState
        | s when s = infectedState -> cleanState
    (pos,newState)

// let updateNodePart2 (node:((int*int)*string)) =
//     let pos,state = node
//     let newState =
//         match state with
//         | cleanState -> weakenedState
//         | weakenedState -> infectedState
//         | infectedState -> flaggedState
//         | flaggedState -> cleanState
//     (pos,newState)

let getNode (grid:Map<(int32*int32),string>) (pos:(int*int)) =
    match grid.TryFind pos with
    | None -> (pos,cleanState)
    | Some s -> (pos,s)

let getState (node:((int*int)*string)) =
    let _,state = node
    state

let printGrid (grid:string[][]) =

    for i in (grid.Length - 1) .. -1 .. 0 do
       grid.[i] |> Array.iter (fun cell ->
                let c = cell
                printf "%s " c)
       printfn ""

let rec step steps infections (carrier:((int*int)*(int*int))) (grid:Map<(int32*int32),string>) =
    let pos,dir = carrier
    let pos_x,pos_y = pos

    if steps <= 0 then
        infections
    else

    let newInfection =
        if getNode grid pos |> getState = infectedState
        then 0
        else 1

    let newDir = if newInfection = 0 then right dir else left dir
    let updatedNode = updateNodePart1 (pos, if grid.ContainsKey(pos) then grid.[(pos)] else cleanState)
    let newGrid = grid.Add(updatedNode)

    let newX,newY = newDir
    let newPos = (pos_x + newX , pos_y + (newY))

    step (steps - 1) (infections + newInfection) (newPos,newDir)  newGrid

[<EntryPoint>]
let main argv =

    //let testInput = readLines "testinput.txt" |> Seq.toArray
    //Array.Reverse(testInput)
    let input = readLines "input.txt" |> Seq.toArray
    Array.Reverse(input)
    let iterations =  10000

    let grid = inputToGrid input

    printGrid grid

    let width = grid.[0].Length + (grid.[0].Length % 2) - 1
    let height = grid.Length + (grid.Length % 2) - 1

    let start = (width / 2, height / 2)
    let direction = (0,1)

    let dict = gridToDict grid

    let bursts = step iterations 0 (start,direction) dict

    printfn "infections: %i" bursts

    // Console.ReadLine() |> ignore

    0 // return an integer exit code
