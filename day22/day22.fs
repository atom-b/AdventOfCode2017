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

let reverse dir =
    dir |> left |> left

let nextState1 (node:((int*int)*string)) =
    let pos,state = node
    let newState =
        match state with
        | s when s = cleanState -> infectedState
        | s when s = infectedState -> cleanState
        | _ -> cleanState
    (pos,newState)

let nextState2 (node:((int*int)*string)) =
    let pos,state = node
    let newState =
        match state with
        | s when s =  cleanState -> weakenedState
        | s when s =  weakenedState -> infectedState
        | s when s =  infectedState -> flaggedState
        | s when s =  flaggedState -> cleanState
        | _ -> cleanState
    (pos,newState)

let turn (node:((int*int)*string)) dir =
    let _,state = node

    match state with
    | s when s = cleanState -> left dir
    | s when s = weakenedState -> dir
    | s when s = infectedState -> right dir
    | s when s = flaggedState -> reverse dir
    | _ -> dir

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

let rec burst remaining infections (carrier:((int*int)*(int*int))) (grid:Map<(int32*int32),string>) switchNode =

    if remaining <= 0 then
        infections
    else

    let pos,dir = carrier
    let posX,posY = pos
    let node = getNode grid pos
    let newNode = switchNode node
    let newGrid = grid.Add <| newNode

    let newInfection =
        if newNode |> getState = infectedState
        then 1
        else 0

    let newDir = turn node dir
    let newX,newY = newDir
    let newPos = (posX + newX, posY + newY)

    burst (remaining - 1) (infections + newInfection) (newPos,newDir) newGrid switchNode

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

    let infectionsP1 = burst iterations 0 (start,direction) dict nextState1

    printfn "infections: %i" infectionsP1

    assert (infectionsP1 = 5575)

    let iterationsP2 = 10000000
    let infectionsP2 = burst iterationsP2 0 (start,direction) dict nextState2
    printfn "infections 2: %i" infectionsP2

    printfn "%A" DateTime.Now

    // Console.ReadLine() |> ignore

    0 // return an integer exit code
