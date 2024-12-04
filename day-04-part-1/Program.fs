// "Looks like the Chief's not here. Next!" One of The Historians pulls out a device and pushes the only button on it. After a brief flash, you recognize the interior of the Ceres monitoring station!
//
// As the search for the Chief continues, a small Elf who lives on the station tugs on your shirt; she'd like to know if you could help her with her word search (your puzzle input). She only has to find one word: XMAS.
//
// This word search allows words to be horizontal, vertical, diagonal, written backwards, or even overlapping other words. It's a little unusual, though, as you don't merely need to find one instance of XMAS - you need to find all of them. Here are a few ways XMAS might appear, where irrelevant characters have been replaced with .:
//
// ..X...
// .SAMX.
// .A..A.
// XMAS.S
// .X....
//
// The actual word search will be full of letters instead. For example:
//
// MMMSXXMASM
// MSAMXMSMSA
// AMXSXMAAMM
// MSAMASMSMX
// XMASAMXAMM
// XXAMMXXAMA
// SMSMSASXSS
// SAXAMASAAA
// MAMMMXMMMM
// MXMXAXMASX
//
// In this word search, XMAS occurs a total of 18 times; here's the same word search again, but where letters not involved in any XMAS have been replaced with .:
//
// ....XXMAS.
// .SAMXMS...
// ...S..A...
// ..A.A.MS.X
// XMASAMX.MM
// X.....XA.A
// S.S.S.S.SS
// .A.A.A.A.A
// ..M.M.M.MM
// .X.X.XMASX
//
// Take a look at the little Elf's word search. How many times does XMAS appear?

open System.IO

type Point = {
    X: int
    Y: int
}

type Direction =
    | North
    | NorthEast
    | East
    | SouthEast
    | South
    | SouthWest
    | West
    | NorthWest

let allDirections = [
    Direction.North
    Direction.NorthEast
    Direction.East
    Direction.SouthEast
    Direction.South
    Direction.SouthWest
    Direction.West
    Direction.NorthWest
]

type Candidate = {
    Point: Point
    Direction: Direction
}

let rawMap = File.ReadAllLines "./input.txt"

// let rawMapStr = @"MMMSXXMASM
// MSAMXMSMSA
// AMXSXMAAMM
// MSAMASMSMX
// XMASAMXAMM
// XXAMMXXAMA
// SMSMSASXSS
// SAXAMASAAA
// MAMMMXMMMM
// MXMXAXMASX"

// let rawMap = rawMapStr.Split(Environment.NewLine)

let sizeX = rawMap |> Array.head |> Seq.length
let sizeY  = rawMap |> Array.length

let map = Array2D.create sizeX sizeY '.'

rawMap
    |> Array.iteri
        (fun yIndex line ->
            line
                |> Seq.iteri
                    (fun xIndex ch -> map[xIndex, yIndex] <- ch))

let term = "XMAS"

let roots =
    [for x in 0..(sizeX-1) do for y in 0..(sizeY-1) -> (x, y)]
        |> List.choose (fun (x, y) -> if map[x, y] = term[0] then Some { X = x; Y = y } else None)

let pointForDirection direction point =
    match direction with
    | Direction.North when point.Y > 0                                    -> Some { X = point.X; Y = point.Y - 1 }
    | Direction.NorthEast when point.Y > 0 && point.X < (sizeX-1)         -> Some { X = point.X + 1; Y = point.Y - 1 }
    | Direction.East when point.X < (sizeX-1)                             -> Some { X = point.X + 1; Y = point.Y }
    | Direction.SouthEast when point.X < (sizeX-1) && point.Y < (sizeY-1) -> Some { X = point.X + 1; Y = point.Y + 1 }
    | Direction.South when point.Y < (sizeY-1)                            -> Some { X = point.X; Y = point.Y + 1 }
    | Direction.SouthWest when point.Y < (sizeY-1) && point.X > 0         -> Some { X = point.X - 1; Y = point.Y + 1 }
    | Direction.West when point.X > 0                                     -> Some { X = point.X - 1; Y = point.Y }
    | Direction.NorthWest when point.X > 0 && point.Y > 0                 -> Some { X = point.X - 1; Y = point.Y - 1 }
    | _                                                                   -> None

let startingRuns =
    let nextChar = term[1]

    roots
        |> List.collect
            (fun root ->
                allDirections
                    |> List.choose
                        (fun direction ->
                            let nextPoint = pointForDirection direction root

                            match nextPoint with
                            | Some p ->
                                if map[p.X, p.Y] = nextChar then Some { Point = root; Direction = direction } else None
                            | _ -> None))

let runMatchesTerm candidate =
    let rec matchInner text point =
        if Seq.isEmpty text then
            true
        elif map[point.X, point.Y] <> (Seq.head text) then
            false
        else
            let nextPoint = pointForDirection candidate.Direction point
            let nextText = (Seq.tail text)

            match nextPoint with
            | Some p -> matchInner nextText p
            | _ -> Seq.isEmpty nextText

    matchInner term candidate.Point

let matches = startingRuns |> List.filter runMatchesTerm

printfn "%A" (matches.Length)
