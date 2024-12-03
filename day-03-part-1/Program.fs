// "Our computers are having issues, so I have no idea if we have any Chief Historians in stock! You're welcome to
// check the warehouse, though," says the mildly flustered shopkeeper at the North Pole Toboggan Rental Shop. The
// Historians head out to take a look.
//
// The shopkeeper turns to you. "Any chance you can see why our computers are having issues again?"
//
// The computer appears to be trying to run a program, but its memory (your puzzle input) is corrupted. All of the
// instructions have been jumbled up!
//
// It seems like the goal of the program is just to multiply some numbers. It does that with instructions like mul(X,Y),
// where X and Y are each 1-3 digit numbers. For instance, mul(44,46) multiplies 44 by 46 to get a result of 2024. Similarly,
// mul(123,4) would multiply 123 by 4.
//
// However, because the program's memory has been corrupted, there are also many invalid characters that should be ignored,
// even if they look like part of a mul instruction. Sequences like mul(4*, mul(6,9!, ?(12,34), or mul ( 2 , 4 ) do nothing.
//
// For example, consider the following section of corrupted memory:
//
// xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
//
// Only the four highlighted sections are real mul instructions. Adding up the result of each instruction produces 161 (2*4 + 5*5 + 11*8 + 8*5).
//
// Scan the corrupted memory for uncorrupted mul instructions. What do you get if you add up all of the results of the multiplications?

open System
open System.IO
open System.Text.RegularExpressions

let input = String.Join("", File.ReadAllLines "./input.txt")

// let input = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

let (|Regexer|_|) (regex : Regex) input =
    let m = regex.Match(input)

    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parserRegex = Regex(@"(mul\(\d{1,3},\d{1,3}\))(.*)$")
let instructionRegex = Regex(@"mul\((\d{1,3}),(\d{1,3})\)")

let parseInstructions text =
    let rec innerParse text results =
        let m = parserRegex.Match(text)

        if m.Success then
            innerParse m.Groups[2].Value (m.Groups[1].Value :: results)
        else
            results

    innerParse text [] |> List.rev

let instructions = parseInstructions input

let result =
    instructions
        |> List.map
            (fun item ->
                match item with
                | Regexer instructionRegex [x1; x2] -> int(x1) * int(x2)
                | _ -> raise <| ApplicationException($"Unexpected instruction: {item}"))
        |> List.sum

printfn "%A" result
