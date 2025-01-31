﻿// The Historians push the button on their strange device, but this time, you all just feel like you're falling.
//
// "Situation critical", the device announces in a familiar voice. "Bootstrapping process failed. Initializing debugger...."
//
// The small handheld device suddenly unfolds into an entire computer! The Historians look around nervously before one
// of them tosses it to you.
//
// This seems to be a 3-bit computer: its program is a list of 3-bit numbers (0 through 7), like 0,1,2,3. The computer also
// has three registers named A, B, and C, but these registers aren't limited to 3 bits and can instead hold any integer.
//
// The computer knows eight instructions, each identified by a 3-bit number (called the instruction's opcode). Each instruction
// also reads the 3-bit number after it as an input; this is called its operand.
//
// A number called the instruction pointer identifies the position in the program from which the next opcode will be read; it
// starts at 0, pointing at the first 3-bit number in the program. Except for jump instructions, the instruction pointer increases
// by 2 after each instruction is processed (to move past the instruction's opcode and its operand). If the computer tries to read
// an opcode past the end of the program, it instead halts.
//
// So, the program 0,1,2,3 would run the instruction whose opcode is 0 and pass it the operand 1, then run the instruction having
// opcode 2 and pass it the operand 3, then halt.
//
// There are two types of operands; each instruction specifies the type of its operand. The value of a literal operand is the
// operand itself. For example, the value of the literal operand 7 is the number 7. The value of a combo operand can be found as follows:
//
// * Combo operands 0 through 3 represent literal values 0 through 3.
// * Combo operand 4 represents the value of register A.
// * Combo operand 5 represents the value of register B.
// * Combo operand 6 represents the value of register C.
// * Combo operand 7 is reserved and will not appear in valid programs.
//
// The eight instructions are as follows:
//
// The adv instruction (opcode 0) performs division. The numerator is the value in the A register. The denominator is found by
// raising 2 to the power of the instruction's combo operand. (So, an operand of 2 would divide A by 4 (2^2); an operand of 5
// would divide A by 2^B.) The result of the division operation is truncated to an integer and then written to the A register.
//
// The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand, then stores
// the result in register B.
//
// The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3 bits),
// then writes that value to the B register.
//
// The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A register is not zero, it jumps by setting
// the instruction pointer to the value of its literal operand; if this instruction jumps, the instruction pointer is not increased
// by 2 after this instruction.
//
// The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C, then stores the result in register B. (For
// legacy reasons, this instruction reads an operand but ignores it.)
//
// The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs that value. (If a program
// outputs multiple values, they are separated by commas.)
//
// The bdv instruction (opcode 6) works exactly like the adv instruction except that the result is stored in the B register. (The
// numerator is still read from the A register.)
//
// The cdv instruction (opcode 7) works exactly like the adv instruction except that the result is stored in the C register. (The
// numerator is still read from the A register.)
//
// Here are some examples of instruction operation:
//
// * If register C contains 9, the program 2,6 would set register B to 1.
// * If register A contains 10, the program 5,0,5,1,5,4 would output 0,1,2.
// * If register A contains 2024, the program 0,1,5,4,3,0 would output 4,2,5,6,7,7,7,7,3,1,0 and leave 0 in register A.
// * If register B contains 29, the program 1,7 would set register B to 26.
// * If register B contains 2024 and register C contains 43690, the program 4,0 would set register B to 44354.
//
// The Historians' strange device has finished initializing its debugger and is displaying some information about the program it is
// trying to run (your puzzle input). For example:
//
// Register A: 729
// Register B: 0
// Register C: 0
//
// Program: 0,1,5,4,3,0
//
// Your first task is to determine what the program is trying to output. To do this, initialize the registers to the given values,
// then run the given program, collecting any output produced by out instructions. (Always join the values produced by out instructions
// with commas.) After the above program halts, its final output will be 4,6,3,5,6,3,5,2,1,0.
//
// Using the information provided by the debugger, initialize the registers to the given values, then run the program. Once it halts,
// what do you get if you use commas to join the values it output into a single string?

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

let rawInput = File.ReadAllText("./input.txt")

// let rawInput = @"Register A: 729
// Register B: 0
// Register C: 0

// Program: 0,1,5,4,3,0"

let registers =
    match rawInput with
    | Regexer registerRegex [a; b; c] -> { A = int a; B = int b; C = int c }
    | _ -> raise <| ApplicationException($"Invalid registers format within input: {rawInput}")

let program =
    match rawInput with
    | Regexer programRegex [p] ->
        let bytes = p.Split(',')
        let bytePairs = bytes |> Array.chunkBySize 2

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


let executeProgram registers program =
    let mutable context = { Registers = registers; Program = program; InstructionPointer = 0 }
    let mutable output = []
    let isHalted () = context.InstructionPointer >= context.Program.Length

    while not <| isHalted () do
        let currentInstruction = program[context.InstructionPointer]
        let (newRegisters, newOutput, newInstructionPointer) = executeOpcode context.Registers currentInstruction

        newOutput |> Option.iter (fun out -> output <- out :: output)

        context <-
            { context with
                Registers = newRegisters
                InstructionPointer = newInstructionPointer |> Option.defaultWith (fun () -> context.InstructionPointer + 1)}

    (output |> List.rev, context.Registers)

let (output, _) = executeProgram registers program
let results = String.Join(',', output)

printfn "\n\noutput = %s" results
