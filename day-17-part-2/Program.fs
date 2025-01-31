// Digging deeper in the device's manual, you discover the problem: this program is supposed to output another
// copy of the program! Unfortunately, the value in register A seems to have been corrupted. You'll need to find
// a new value to which you can initialize register A so that the program's output instructions produce an exact
// copy of the program itself.
//
// For example:
//
// Register A: 2024
// Register B: 0
// Register C: 0
//
// Program: 0,3,5,4,3,0
//
// This program outputs a copy of itself if register A is instead initialized to 117440. (The original initial value
// of register A, 2024, is ignored.)
//
// What is the lowest positive initial value for register A that causes the program to output a copy of itself?

// idea: pass into the executeProgram function the output we're looking for, and if at any point it does not match, then
//       stop going further with that particular register value.  might also need to put a cap on execution count to
//       avoid infinite loops.

open System
open System.IO
open System.Text.RegularExpressions
open Common

type Opcode =
| Adv = 0
| Bxl = 1
| Bst = 2
| Jnz = 3
| Bxc = 4
| Out = 5
| Bdv = 6
| Cdv = 7

type Instruction = {
    Opcode: Opcode
    Operand: int
}

type Registers = {
    A: int
    B: int
    C: int
}

type ExecutionContext = {
    Registers: Registers
    Program: Instruction list
    InstructionPointer: int
}

let registerRegex = Regex(@"Register\sA:\s(\d+)\s+Register\sB:\s(\d+)\s+Register\sC:\s(\d+)")
let programRegex = Regex(@"Program: ([\d,]+)")

let rawInput = File.ReadAllText("../day-17-part-1/input.txt")

// let rawInput = @"Register A: 2024
// Register B: 0
// Register C: 0

// Program: 0,3,5,4,3,0"

let registers =
    match rawInput with
    | Regexer registerRegex [a; b; c] -> { A = int a; B = int b; C = int c }
    | _ -> raise <| ApplicationException($"Invalid registers format within input: {rawInput}")

let (program, rawProgram) =
    match rawInput with
    | Regexer programRegex [p] ->
        let bytes = p.Split(',')
        let bytePairs = bytes |> Array.chunkBySize 2
        let intBytes = bytes |> Array.map int |> List.ofArray

        let program =
            bytePairs
            |> Array.map
                (fun pair ->
                    match pair with
                    | [|rawOpcode; operand|] ->
                        let opcode =
                            if Enum.IsDefined (typedefof<Opcode>, int rawOpcode) then
                                enum<Opcode> (int rawOpcode)
                            else
                                raise <| ApplicationException($"Invalid opcode value: {rawOpcode}")

                        { Opcode = opcode; Operand = int operand }
                    | _ ->
                        let text = sprintf "%A" pair
                        raise <| ApplicationException($"Invalid instruction pair: {text}"))
            |> List.ofArray

        (program, intBytes)
    | _ -> raise <| ApplicationException($"Invalid program format within input: {rawInput}")

printfn "registers =\n%A\n\nprogram =\n%A" registers program

let executeOpcode registers instruction =
    let combo operand =
        match operand with
        | x when x >= 0 && x <= 3 -> x
        | 4 -> registers.A
        | 5 -> registers.B
        | 6 -> registers.C
        | _ ->
            let formattedInstruction = sprintf "%A" instruction
            raise <| ApplicationException($"Invalid combo operand ({operand}) in instruction: {formattedInstruction}")

    match instruction.Opcode with
    | Opcode.Adv ->
        let newA = registers.A / (pown 2 (combo instruction.Operand)) // may need to use something more efficient that pown later on
        ({ registers with A = newA }, None, None)
    | Opcode.Bxl ->
        let newB = registers.B ^^^ instruction.Operand
        ({ registers with B = newB }, None, None)
    | Opcode.Bst ->
        let newB = (combo instruction.Operand) % 8
        ({ registers with B = newB }, None, None)
    | Opcode.Jnz ->
        let newInstructionPointer = if registers.A <> 0 then Some (instruction.Operand / 2) else None
        (registers, None, newInstructionPointer)
    | Opcode.Bxc ->
        let newB = registers.B ^^^ registers.C
        ({ registers with B = newB }, None, None)
    | Opcode.Out ->
        let output = (combo instruction.Operand) % 8
        (registers, Some output, None)
    | Opcode.Bdv ->
        let newB = registers.A / (pown 2 (combo instruction.Operand)) // may need to use something more efficient that pown later on
        ({ registers with B = newB }, None, None)
    | Opcode.Cdv ->
        let newC = registers.A / (pown 2 (combo instruction.Operand)) // may need to use something more efficient that pown later on
        ({ registers with C = newC }, None, None)
    | _ -> raise <| ApplicationException($"Invalid instruction opcode: {instruction.Opcode}")


let executeProgram registers program requiredOutput =
    let mutable context = { Registers = registers; Program = program; InstructionPointer = 0 }
    let mutable output = []
    let isHalted () = context.InstructionPointer >= context.Program.Length

    while not <| isHalted () do
        let currentInstruction = program[context.InstructionPointer]
        let (newRegisters, newOutput, newInstructionPointer) = executeOpcode context.Registers currentInstruction

        if Option.isSome newOutput then
            newOutput |> Option.iter (fun out -> output <- out :: output)
            let satisfiesRequiredOutput = (Seq.ofList output |> Seq.rev, Seq.ofList requiredOutput) ||> Seq.forall2 (fun x1 x2 -> x1 = x2)

            if not satisfiesRequiredOutput then
                // printfn ">>> output = %A, expected = %A" output requiredOutput
                raise <| ApplicationException($"Register A did not satisfy required output: {registers.A}")

        context <-
            { context with
                Registers = newRegisters
                InstructionPointer = newInstructionPointer |> Option.defaultWith (fun () -> context.InstructionPointer + 1)}

    (output |> List.rev, context.Registers)

let findLowestRegisterA program rawProgram startA endA=
    let mutable i = startA
    let mutable isFound = false

    while i <= endA && not isFound do
        try
            try
                if i % 1000000 = 0 then
                    printfn ">>> attempting A = %d" i

                let (output, _) = executeProgram { A = i; B = 0; C = 0 } program rawProgram

                if output = rawProgram then
                    isFound <- true
            with
            | _ -> () // Consume exception and continue
        finally
            if not isFound then i <- i + 1

    if isFound then Some i else None

// let (output, _) = executeProgram { A = 117440; B = 0; C = 0 } program rawProgram
// let results = String.Join(',', output)

// printfn "\n\noutput =\n%A" results

let args = Environment.GetCommandLineArgs() |> Array.skip 1

if args.Length < 1 then
    raise <| ApplicationException("Too many command-line arguments.")

let section = int args[0]

if section < 1 || section > 5 then
    raise <| ApplicationException($"Invalid section value: {section}")

let endA =
    (fun x ->
        let e = x * 1000000000L
        if e > Int32.MaxValue then Int32.MaxValue else int e) section
let startA = (section - 1) * 1000000000

printfn "\n\nstart = %d, end = %d" startA endA

let results = findLowestRegisterA program rawProgram startA endA

printfn "\n\nlowest = %A" results
