open System

let left = -1
let right = 1

type Rule = {
    Condition: int;
    WriteVal: int;
    MoveDir: int;
    NextStateId: string
}

type State = {
    Rules: Map<int,Rule>
}

let rec step stepsRemaining (program:Map<string,State>) stateId pos (tape:Map<int,int>) =
    if stepsRemaining <= 0 then tape else

    let currentVal = defaultArg (tape.TryFind pos) 0
    let branch = program.[stateId].Rules.[currentVal]

    tape.Add (pos, branch.WriteVal) |> step (stepsRemaining - 1) program branch.NextStateId (pos + branch.MoveDir)

let testProgram =

    [
        ("A",
            { Rules = [
                (0, {Condition = 0; WriteVal = 1; MoveDir = right; NextStateId = "B";});
                (1, {Condition = 1; WriteVal = 0; MoveDir = left; NextStateId = "B";});]
                |> Map.ofSeq
            });
        ("B", { Rules = [
            (0, {Condition = 0; WriteVal = 1; MoveDir = left; NextStateId = "A";});
            (1, {Condition = 1; WriteVal = 1; MoveDir = right; NextStateId = "A";});]
            |> Map.ofSeq });
    ] |> Map.ofSeq

let inputProgram =
    [
        ("A", { Rules = [
            (0, {Condition = 0; WriteVal = 1; MoveDir = right; NextStateId = "B";});
            (1, {Condition = 1; WriteVal = 0; MoveDir = left; NextStateId = "B";});]
            |> Map.ofSeq });
        ("B", { Rules = [
            (0, {Condition = 0; WriteVal = 0; MoveDir = right; NextStateId = "C";});
            (1, {Condition = 1; WriteVal = 1; MoveDir = left; NextStateId = "B";});]
            |> Map.ofSeq });
        ("C", { Rules = [
            (0, {Condition = 0; WriteVal = 1; MoveDir = right; NextStateId = "D";});
            (1, {Condition = 1; WriteVal = 0; MoveDir = left; NextStateId = "A";});]
            |> Map.ofSeq });

        ("D", { Rules = [
            (0, {Condition = 0; WriteVal = 1; MoveDir = left; NextStateId = "E";});
            (1, {Condition = 1; WriteVal = 1; MoveDir = left; NextStateId = "F";});]
            |> Map.ofSeq });

        ("E", { Rules = [
            (0, {Condition = 0; WriteVal = 1; MoveDir = left; NextStateId = "A";});
            (1, {Condition = 1; WriteVal = 0; MoveDir = left; NextStateId = "D";});]
            |> Map.ofSeq });

        ("F", { Rules = [
            (0, {Condition = 0; WriteVal = 1; MoveDir = right; NextStateId = "A";});
            (1, {Condition = 1; WriteVal = 1; MoveDir = left; NextStateId = "E";});]
            |> Map.ofSeq });
    ] |> Map.ofSeq

[<EntryPoint>]
let main argv =

    let startState = "A"
    let iterations = 12629077

    let tape = step iterations inputProgram startState 0 ([(0,0)]|> Map.ofSeq)
    let checksum = tape |> Map.fold (fun acc key tapeSlot -> acc + tapeSlot) 0
    printfn "checksum: %i" checksum

    0