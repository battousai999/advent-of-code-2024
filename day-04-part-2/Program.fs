// The Elf looks quizzically at you. Did you misunderstand the assignment?
//
// Looking for the instructions, you flip over the word search to find that this isn't actually an XMAS puzzle; it's an X-MAS puzzle in which you're supposed to find two MAS in the shape of an X. One way to achieve that is like this:
//
// M.S
// .A.
// M.S
//
// Irrelevant characters have again been replaced with . in the above diagram. Within the X, each MAS can be written forwards or backwards.
//
// Here's the same example from before, but this time all of the X-MASes have been kept instead:
//
// .M.S......
// ..A..MSMS.
// .M.S.MAA..
// ..A.ASMSM.
// .M.S.M....
// ..........
// S.S.S.S.S.
// .A.A.A.A..
// M.M.M.M.M.
// ..........
//
// In this example, an X-MAS appears 9 times.
//
// Flip the word search from the instructions back over to the word search side and try again. How many times does an X-MAS appear?

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

let rawMap = File.ReadAllLines "../day-04-part-1/input.txt"

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

let roots =
    [for x in 0..(sizeX-1) do for y in 0..(sizeY-1) -> (x, y)]
        |> List.choose (fun (x, y) -> if map[x, y] = 'A' then Some { X = x; Y = y } else None)

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

let rootIsXmas point =
    let charNorthWest = pointForDirection Direction.NorthWest point |> Option.map (fun p -> map[p.X, p.Y])
    let charNorthEast = pointForDirection Direction.NorthEast point |> Option.map (fun p -> map[p.X, p.Y])
    let charSouthEast = pointForDirection Direction.SouthEast point |> Option.map (fun p -> map[p.X, p.Y])
    let charSouthWest = pointForDirection Direction.SouthWest point |> Option.map (fun p -> map[p.X, p.Y])

    let firstDiagonalSatisfied =
        match (charNorthWest, charSouthEast) with
        | (Some 'M', Some 'S') -> true
        | (Some 'S', Some 'M') -> true
        | _ -> false

    let secondDiagonalSatisfied =
        match (charNorthEast, charSouthWest) with
        | (Some 'M', Some 'S') -> true
        | (Some 'S', Some 'M') -> true
        | _ -> false

    firstDiagonalSatisfied && secondDiagonalSatisfied

let matches = roots |> List.filter rootIsXmas

printfn "%A" (matches.Length)
