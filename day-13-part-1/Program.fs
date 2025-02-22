﻿// Next up: the lobby of a resort on a tropical island. The Historians take a moment to admire the hexagonal
// floor tiles before spreading out.
//
// Fortunately, it looks like the resort has a new arcade! Maybe you can win some prizes from the claw machines?
//
// The claw machines here are a little unusual. Instead of a joystick or directional buttons to control the claw,
// these machines have two buttons labeled A and B. Worse, you can't just put in a token and play; it costs 3 tokens
// to push the A button and 1 token to push the B button.
//
// With a little experimentation, you figure out that each machine's buttons are configured to move the claw a specific
// amount to the right (along the X axis) and a specific amount forward (along the Y axis) each time that button is pressed.
//
// Each machine contains one prize; to win the prize, the claw must be positioned exactly above the prize on both the X and Y axes.
//
// You wonder: what is the smallest number of tokens you would have to spend to win as many prizes as possible? You assemble
// a list of every machine's button behavior and prize location (your puzzle input). For example:
//
// Button A: X+94, Y+34
// Button B: X+22, Y+67
// Prize: X=8400, Y=5400
//
// Button A: X+26, Y+66
// Button B: X+67, Y+21
// Prize: X=12748, Y=12176
//
// Button A: X+17, Y+86
// Button B: X+84, Y+37
// Prize: X=7870, Y=6450
//
// Button A: X+69, Y+23
// Button B: X+27, Y+71
// Prize: X=18641, Y=10279
//
// This list describes the button configuration and prize location of four different claw machines.
//
// For now, consider just the first claw machine in the list:
//
// - Pushing the machine's A button would move the claw 94 units along the X axis and 34 units along the Y axis.
// - Pushing the B button would move the claw 22 units along the X axis and 67 units along the Y axis.
// - The prize is located at X=8400, Y=5400; this means that from the claw's initial position, it would need to move exactly
//   8400 units along the X axis and exactly 5400 units along the Y axis to be perfectly aligned with the prize in this machine.
//
// The cheapest way to win the prize is by pushing the A button 80 times and the B button 40 times. This would line up the claw
// along the X axis (because 80*94 + 40*22 = 8400) and along the Y axis (because 80*34 + 40*67 = 5400). Doing this would cost 80*3
// tokens for the A presses and 40*1 for the B presses, a total of 280 tokens.
//
// For the second and fourth claw machines, there is no combination of A and B presses that will ever win a prize.
//
// For the third claw machine, the cheapest way to win the prize is by pushing the A button 38 times and the B button 86 times. Doing
// this would cost a total of 200 tokens.
//
// So, the most prizes you could possibly win is two; the minimum tokens you would have to spend to win all (two) prizes is 480.
//
// You estimate that each button would need to be pressed no more than 100 times to win a prize. How else would someone be expected to play?
//
// Figure out how to win as many prizes as possible. What is the fewest tokens you would have to spend to win all possible prizes?

open System
open System.IO
open Common
open System.Text.RegularExpressions

type Point = {
    X: int
    Y: int
}

type ClawMachine = {
    ButtonA: Point
    ButtonB: Point
    PrizeLocation: Point
}

let machineRegexPattern = @"^Button\sA:\sX\+(\d+),\sY\+(\d+)[\n\r]+Button\sB:\sX\+(\d+),\sY\+(\d+)[\n\r]+Prize:\sX=(\d+),\sY=(\d+)"

let machineRegex = Regex(machineRegexPattern)

let rawMachinesStr = File.ReadAllLines "./input.txt"

// let rawMachinesInitialStr = @"Button A: X+94, Y+34
// Button B: X+22, Y+67
// Prize: X=8400, Y=5400

// Button A: X+26, Y+66
// Button B: X+67, Y+21
// Prize: X=12748, Y=12176

// Button A: X+17, Y+86
// Button B: X+84, Y+37
// Prize: X=7870, Y=6450

// Button A: X+69, Y+23
// Button B: X+27, Y+71
// Prize: X=18641, Y=10279"

// let rawMachinesStr = rawMachinesInitialStr.Split(Environment.NewLine)

let rawMachines = rawMachinesStr |> Array.chunkBySize 4 |> Array.map (fun ls -> String.Join(Environment.NewLine, ls))

let machines =
    rawMachines
        |> Array.map
            (fun line ->
                match line with
                | Regexer machineRegex [ax; ay; bx; by; px; py] ->
                    { ButtonA = { X = int ax; Y = int ay }; ButtonB = { X = int bx; Y = int by }; PrizeLocation = { X = int px; Y = int py }}
                | _ -> raise <| ApplicationException($"Unexpected machine string: {line}"))
        |> List.ofArray

let findSolutions machine =
    let a = machine.ButtonA
    let b = machine.ButtonB
    let p = machine.PrizeLocation
    let min a b = if a < b then a else b
    let maxN = Math.Floor(Math.Min((float p.X / float a.X), (float p.Y / float a.Y))) |> int
    let quotient1 n = (p.X - (n * a.X)) / b.X
    let remainder1 n = (p.X - (n * a.X)) % b.X
    let quotient2 n = (p.Y - (n * a.Y)) / b.Y
    let remainder2 n = (p.Y - (n * a.Y)) % b.Y

    [0..(min maxN 100)]
        |> List.filter
            (fun n ->
                if remainder1 n = 0 && remainder2 n = 0 then
                    let m1 = quotient1 n
                    let m2 = quotient2 n

                    m1 = m2
                else
                    false)
        |> List.map (fun n -> (n, quotient1 n))

let costFunction (a, b) = (3 * a) + b

let findCheapest (solutions: (int * int) list) = solutions |> List.minBy costFunction

let answer =
    machines
        |> List.collect
            (fun machine ->
                let solutions = findSolutions machine

                if List.isEmpty solutions then
                    []
                else
                    [findCheapest solutions])
        |> List.sumBy costFunction

printfn "%d" answer
