// As you scan through the corrupted memory, you notice that some of the conditional statements are also still
// intact. If you handle some of the uncorrupted conditional statements in the program, you might be able to get
// an even more accurate result.
//
// There are two new instructions you'll need to handle:
//
// - The do() instruction enables future mul instructions.
// - The don't() instruction disables future mul instructions.
//
// Only the most recent do() or don't() instruction applies. At the beginning of the program, mul instructions
// are enabled.
//
// For example:
//
// xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
//
// This corrupted memory is similar to the example from before, but this time the mul(5,5) and mul(11,8)
// instructions are disabled because there is a don't() instruction before them. The other mul instructions
// function normally, including the one at the end that gets re-enabled by a do() instruction.
//
// This time, the sum of the results is 48 (2*4 + 8*5).
//
// Handle the new instructions; what do you get if you add up all of the results of just the enabled multiplications?

open System
open System.IO
open System.Text.RegularExpressions

let input = String.Join("", File.ReadAllLines "../day-03-part-1/input.txt")

// let input = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

let (|Regexer|_|) (regex : Regex) input =
    let m = regex.Match(input)

    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parserRegex = Regex(@"(?:(mul\(\d{1,3},\d{1,3}\))|(do\(\))|(don't\(\)))(.*)$")
let instructionRegex = Regex(@"mul\((\d{1,3}),(\d{1,3})\)")

let parseInstructions text =
    let rec innerParse isEnabled text results =
        let m = parserRegex.Match(text)

        match text with
        | Regexer parserRegex [mulGroup; ""; ""; rest] when mulGroup.Length > 0 ->
            innerParse isEnabled rest (if isEnabled then (mulGroup :: results) else results)
        | Regexer parserRegex [""; doGroup; ""; rest] when doGroup.Length > 0 ->
            innerParse true rest results
        | Regexer parserRegex [""; ""; dontGroup; rest] when dontGroup.Length > 0 ->
            innerParse false rest results
        | _ -> results

    innerParse true text [] |> List.rev

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
