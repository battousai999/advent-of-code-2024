﻿// Digging deeper in the device's manual, you discover the problem: this program is supposed to output another
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
    A: int64
    B: int64
    C: int64
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
        | x when x >= 0 && x <= 3 -> int64 x
        | 4 -> registers.A
        | 5 -> registers.B
        | 6 -> registers.C
        | _ ->
            let formattedInstruction = sprintf "%A" instruction
            raise <| ApplicationException($"Invalid combo operand ({operand}) in instruction: {formattedInstruction}")

    match instruction.Opcode with
    | Opcode.Adv ->
        let newA = registers.A / (int64 (pown 2L (int (combo instruction.Operand)))) // may need to use something more efficient that pown later on
        ({ registers with A = newA }, None, None)
    | Opcode.Bxl ->
        let newB = registers.B ^^^ instruction.Operand
        ({ registers with B = newB }, None, None)
    | Opcode.Bst ->
        let newB = (combo instruction.Operand) % 8L
        ({ registers with B = newB }, None, None)
    | Opcode.Jnz ->
        let newInstructionPointer = if registers.A <> 0 then Some (instruction.Operand / 2) else None
        (registers, None, newInstructionPointer)
    | Opcode.Bxc ->
        let newB = registers.B ^^^ registers.C
        ({ registers with B = newB }, None, None)
    | Opcode.Out ->
        let output = (int (combo instruction.Operand)) % 8
        (registers, Some (if output < 0 then output + 8 else output), None)
    | Opcode.Bdv ->
        let newB = registers.A / (int64 (pown 2L (int (combo instruction.Operand)))) // may need to use something more efficient that pown later on
        ({ registers with B = newB }, None, None)
    | Opcode.Cdv ->
        let newC = registers.A / (int64 (pown 2L (int (combo instruction.Operand)))) // may need to use something more efficient that pown later on
        ({ registers with C = newC }, None, None)
    | _ -> raise <| ApplicationException($"Invalid instruction opcode: {instruction.Opcode}")


let executeProgram registers program =
    let mutable context = { Registers = registers; Program = program; InstructionPointer = 0 }
    let mutable output = []
    let isHalted () = context.InstructionPointer >= context.Program.Length

    while not <| isHalted () do
        let currentInstruction = program[context.InstructionPointer]
        let (newRegisters, newOutput, newInstructionPointer) = executeOpcode context.Registers currentInstruction

        if Option.isSome newOutput then
            newOutput |> Option.iter (fun out -> output <- out :: output)

        context <-
            { context with
                Registers = newRegisters
                InstructionPointer = newInstructionPointer |> Option.defaultWith (fun () -> context.InstructionPointer + 1)}

    (output |> List.rev, context.Registers)

let findLowestRegisterABackwards (rawProgram: int list) program =
    let mutable candidates: int64 list = [0]
    let reverseProgram = rawProgram |> List.rev
    let octalDigits = [0..7]

    reverseProgram
    |> getListSequences
    |> List.map List.rev
    |> List.iter
        (fun sectionToMatch ->
            let mutable newCandidates = []

            candidates
            |> List.iter
                (fun candidate ->
                    octalDigits
                    |> List.iter
                        (fun digit ->
                            let candidateAsOctal = Convert.ToString(candidate, 8)
                            let targetAsOctal = candidateAsOctal + Convert.ToString(digit, 8)
                            let target = Convert.ToInt64(targetAsOctal, 8)

                            try
                                let (output, _) = executeProgram { A = target; B = 0; C = 0 } program

                                if sectionToMatch = output then
                                    newCandidates <- target :: newCandidates
                            with
                            | _ -> ()
                            ))

            candidates <- newCandidates)

    candidates |> List.min

// Target: 88714211294040, 10100001010111101100011000011001111101101011000

// [              7,               7,                                              111] output =               2,                                               10
// [             15,              17,                                             1111] output =              24,                                            10100
// [          15375,           36017,                                   11110000001111] output =           24127,                                   10100001010111
// [          80911,          236017,                                10011110000001111] output =          241275,                                10100001010111101
// [         343055,         1236017,                              1010011110000001111] output =         2412754,                             10100001010111101100


// my program:
//
// b = a % 8            // b = last 3 binary digits of a
// b = b xor 2          // up to this point, b = last 3 binary digits of a with second digit flipped
// c = a / (2 ** b)     // or c = a >> b (i.e., a rightshift b digits)
// b = b xor c
// a = a / (2 ** 3)     // a = a >> 3 (i.e, a loses its last 3 binary digits, getting smaller for each loop)
// b = b xor 7
// out(b % 8)           // what's output will always be the last 3 binary digits of b
// jnz 0                // jump back to beginning until a has lost all of its digits

// Notes:
// * Whatever happens to b during the loop, we only really care about the last 3 digits as it will be %-ed by 8
// * b does not carry over (it will be overwritten at the beginning of each loop)
// * a slowly (3 digits at a time) gets whittled on the right (least-significant digits)
// * c also does not carry over (it gets overwritten near the beginning of the loop)
// * the a = ... line only has a as a dependency, so it could be moved to just before the jump without affecting the program
// * b = (((b xor 2) xor c) xor 7) % 8
// * c = a >> ((a % 8) xor 2)

let results = findLowestRegisterABackwards rawProgram program

printfn "\n\nlowest = %A" results
