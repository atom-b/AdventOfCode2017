open System

let left = -1
let right = 1

type StateBranch(condition:int, writeVal:int, moveDir:int, nextStateId:string) =
    member this.Condition = condition
    member this.WriteVal = writeVal
    member this.MoveDir = moveDir
    member this.NextStateId = nextStateId

type State (branchZero:StateBranch, branchOne:StateBranch) =
    member this.Branches = [(0,branchZero); (1,branchOne)] |> Map.ofSeq

let rec step stepsRemaining (program:Map<string,State>) stateId pos (tape:Map<int,int>) =
    if stepsRemaining <= 0 then tape else

    let currentVal = defaultArg (tape.TryFind pos) 0
    let branch = program.[stateId].Branches.[currentVal]

    tape.Add (pos, branch.WriteVal) |> step (stepsRemaining - 1) program branch.NextStateId (pos + branch.MoveDir)

let testProgram =
    let stateA = State(StateBranch(0, 1, right, "B"), StateBranch(1, 0, left, "B"))
    let stateB = State(StateBranch(0, 1, left, "A"), StateBranch(1, 1, right, "A"))

    [("A", stateA); ("B", stateB)] |> Map.ofSeq

let inputProgram =
    [
        ("A", State(StateBranch(0, 1, right, "B"), StateBranch(1,0,left,"B")));
        ("B", State(StateBranch(0,0,right,"C"), StateBranch(1,1,left,"B")));
        ("C", State(StateBranch(0,1,right,"D"), StateBranch(1,0,left,"A")));
        ("D", State(StateBranch(0,1,left,"E"), StateBranch(1,1,left,"F")));
        ("E", State(StateBranch(0,1,left,"A"), StateBranch(1,0,left,"D")));
        ("F", State(StateBranch(0,1,right,"A"), StateBranch(1,1,left,"E")));
    ] |> Map.ofSeq

[<EntryPoint>]
let main argv =

    let startState = "A"
    let iterations = 12629077

    let tape = step iterations inputProgram startState 0 ([(0,0)]|> Map.ofSeq)
    let checksum = tape |> Map.fold (fun acc key tapeSlot -> acc + tapeSlot) 0
    printfn "checksum: %i" checksum

    0