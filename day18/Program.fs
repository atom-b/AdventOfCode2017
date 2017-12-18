open System

let tableToAsm (t:string) =
    t.Split("\r\n") |> Array.map (fun l -> l.Split([|' '|], StringSplitOptions.None)) |> Array.map (fun a -> (a.[0], a.[1], if a.Length = 3 then a.[2] else ""))

let printJaggedArray arr = 
    arr |> Array.iter (fun r -> printfn "row: %A" r)
    
let mutable lastFreq = 0
let mutable registers = Map.empty<string,int64>

let regOrConst v = 
    let isConst,res = Int64.TryParse v
    if isConst then res else registers.[v]

let setReg reg operand instr =
    let r = registers.TryFind reg
    registers <- 
        match r with
        | Some k -> 
            registers.Remove(reg)
                .Add (reg, (instr operand k))
        | None -> registers.Add (reg, (instr operand 0L)) // TODO: swap order of operand and register value

let _snd freq =
    printfn "snd %d" <| regOrConst freq
    lastFreq <- (int (regOrConst freq))
    1

let _set reg v =
    setReg reg (regOrConst v) (fun operand rval -> operand)
    1

let _add reg v =
    setReg reg (regOrConst v) (fun operand rval -> operand + rval)
    1

let _mul reg operand =
    setReg reg (regOrConst operand) (fun operand rval -> operand * rval)
    1

let _mod reg operand = 
    setReg reg (regOrConst operand) (fun operand rval -> rval % operand)
    1

let _rcv operand =
    if (regOrConst operand) <> 0L then 
        printfn "last frequency: %i" lastFreq
        Console.ReadLine() |> ignore
    else ()
    1

let _jgz reg v =
    let test = regOrConst reg
    let operand = regOrConst v
    if ( test > 0L) then (int operand) else 1
        
let rec execute (program:array<(string*string*string)>) pc =
    if pc >= (int64 program.Length) || pc < 0L then 1L else

    let cmd,reg,operand = program.[(int pc)]
    // this doesn't feel like tail recursion...?
    execute program <| pc + (int64 (
        match cmd with
        | "snd" -> _snd reg
        | "set" -> _set reg operand
        | "add" -> _add reg operand
        | "mul" -> _mul reg operand
        | "mod" -> _mod reg operand
        | "rcv" -> _rcv reg
        | "jgz" -> _jgz reg operand
        | _ -> 1 // skip unknown instructions
        ))


[<EntryPoint>]
let main argv =
//    let input = "set a 1
//add a 2
//mul a a
//mod a 5
//snd a
//set a 0
//rcv a
//jgz a -1
//set a 1
//jgz a -2"

    let input = "set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 316
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19"

    let program = tableToAsm input 
    
    program |> printJaggedArray
    
    printfn "return code: %i" <| execute program 0L
    //printfn "registers: %A" registers
    //execute program.[1]
    //printfn "registers: %A" registers

    Console.ReadLine() |> ignore
    0 // return an integer exit code
