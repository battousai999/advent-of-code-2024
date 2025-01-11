// As you go to win the first prize, you discover that the claw is nowhere near where you expected it would be. Due to a unit conversion error in your measurements, the position of every prize is actually 10000000000000 higher on both the X and Y axis!
//
// Add 10000000000000 to the X and Y position of every prize. After making this change, the example above would now look like this:
//
// Button A: X+94, Y+34
// Button B: X+22, Y+67
// Prize: X=10000000008400, Y=10000000005400
//
// Button A: X+26, Y+66
// Button B: X+67, Y+21
// Prize: X=10000000012748, Y=10000000012176
//
// Button A: X+17, Y+86
// Button B: X+84, Y+37
// Prize: X=10000000007870, Y=10000000006450
//
// Button A: X+69, Y+23
// Button B: X+27, Y+71
// Prize: X=10000000018641, Y=10000000010279
// Now, it is only possible to win a prize on the second and fourth claw machines. Unfortunately, it will take many more than 100 presses to do so.
//
// Using the corrected prize coordinates, figure out how to win as many prizes as possible. What is the fewest tokens you would have to spend to win all possible prizes?

open System
open System.IO
open Common
open System.Text.RegularExpressions

type Point = {
    X: int64
    Y: int64
}

type ClawMachine = {
    ButtonA: Point
    ButtonB: Point
    PrizeLocation: Point
}

let machineRegexPattern = @"^Button\sA:\sX\+(\d+),\sY\+(\d+)[\n\r]+Button\sB:\sX\+(\d+),\sY\+(\d+)[\n\r]+Prize:\sX=(\d+),\sY=(\d+)"

let machineRegex = Regex(machineRegexPattern)

let rawMachinesStr = File.ReadAllLines "../day-13-part-1/input.txt"

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

let prizePadding = 10000000000000L

let machines =
    rawMachines
        |> Array.map
            (fun line ->
                match line with
                | Regexer machineRegex [ax; ay; bx; by; px; py] ->
                    {
                        ButtonA = { X = int64 ax; Y = int64 ay };
                        ButtonB = { X = int64 bx; Y = int64 by };
                        PrizeLocation = { X = (int64 px) + prizePadding; Y = (int64 py) + prizePadding }
                    }
                | _ -> raise <| ApplicationException($"Unexpected machine string: {line}"))
        |> List.ofArray

let findSolutions machine =
    let a = machine.ButtonA
    let b = machine.ButtonB
    let p = machine.PrizeLocation
    let min a b = if a < b then a else b
    let max a b = if a > b then a else b
    let maxN = Math.Floor(Math.Min((float p.X / float a.X), (float p.Y / float a.Y))) |> int64
    let quotient1 n = (p.X - (n * a.X)) / b.X
    let remainder1 n = (p.X - (n * a.X)) % b.X
    let quotient2 n = (p.Y - (n * a.Y)) / b.Y
    let remainder2 n = (p.Y - (n * a.Y)) % b.Y
    // This approxN comes from some algebraic manipulation of the equations:
    //    (n * a.x) + (m * b.x) = p.x
    //    (n * a.y) + (m * b.y) = p.y
    let approxN = ((double p.Y / double b.Y) - (double p.X / double b.X)) / ((double a.Y / double b.Y) - (double a.X / double b.X)) |> int64
    let tolerance = 1000000L

    [(max 0L (approxN - tolerance))..(min maxN (approxN + tolerance))]
        |> List.filter
            (fun n ->
                if remainder1 n = 0 && remainder2 n = 0 then
                    let m1 = quotient1 n
                    let m2 = quotient2 n

                    m1 = m2
                else
                    false)
        |> List.map (fun n -> (n, quotient1 n))

let costFunction (a, b) = (3L * a) + b

let findCheapest (solutions: (int64 * int64) list) = solutions |> List.minBy costFunction

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
