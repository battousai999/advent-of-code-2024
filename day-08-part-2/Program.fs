// Watching over your shoulder as you work, one of The Historians asks if you took the effects of resonant harmonics into your calculations.
//
// Whoops!
//
// After updating your model, it turns out that an antinode occurs at any grid position exactly in line with at least two antennas of the same frequency, regardless of distance. This means that some of the new antinodes will occur at the position of each antenna (unless that antenna is the only one of its frequency).
//
// So, these three T-frequency antennas now create many antinodes:
//
// T....#....
// ...T......
// .T....#...
// .........#
// ..#.......
// ..........
// ...#......
// ..........
// ....#.....
// ..........
//
// In fact, the three T-frequency antennas are all exactly in line with two antennas, so they are all also antinodes! This brings the total number of antinodes in the above example to 9.
//
// The original example now has 34 antinodes, including the antinodes that appear on every antenna:
//
// ##....#....#
// .#.#....0...
// ..#.#0....#.
// ..##...0....
// ....0....#..
// .#...#A....#
// ...#..#.....
// #....#.#....
// ..#.....A...
// ....#....A..
// .#........#.
// ...#......##
//
// Calculate the impact of the signal using this updated model. How many unique locations within the bounds of the map contain an antinode?

open System
open System.IO

let rawMap = File.ReadAllLines "../day-08-part-1/input.txt"

// let rawMapStr = @"............
// ........0...
// .....0......
// .......0....
// ....0.......
// ......A.....
// ............
// ............
// ........A...
// .........A..
// ............
// ............"

// let rawMap = rawMapStr.Split(Environment.NewLine)

type Point = {
    X: int
    Y: int
}

type Antenna = {
    Position: Point
    Frequency: char
}

let antennasArray = ResizeArray()

let sizeX = (rawMap |> Array.head).Length
let sizeY = rawMap |> Array.length

rawMap
    |> Array.iteri
        (fun yIndex line ->
            line
                |> Seq.iteri
                    (fun xIndex ch ->
                        if ch <> '.' then
                            antennasArray.Add({ Frequency = ch; Position = { X = xIndex; Y = yIndex }})))

let antennas = antennasArray |> List.ofSeq

let antennaGroups = antennas |> List.groupBy _.Frequency |> List.filter (fun (_, list) -> list.Length > 1)

let rec pairs list =
    seq {
        match list with
        | x::xs ->
            for element in xs do
                yield (x, element)
            yield! pairs xs
        | _ -> ()
    } |> List.ofSeq

let antennaGroupPairings = antennaGroups |> List.map (fun (ch, list) -> (ch, list |> pairs))

let infiniteSeq () =
    let rec innerSeq n =
        seq {
            yield n
            yield! innerSeq (n + 1L)
        }

    innerSeq 1L

let isPointInBounds point = (point.X >= 0 && point.X < sizeX) && (point.Y >= 0 && point.Y < sizeY)

let getAntinodes point1 point2 =
    let antinodeList1 =
        infiniteSeq()
            |> Seq.map (fun i -> { X = point1.X + (int i) * (point1.X - point2.X); Y = point1.Y + (int i) * (point1.Y - point2.Y) })
            |> Seq.takeWhile isPointInBounds

    let antinodeList2 =
        infiniteSeq()
            |> Seq.map (fun i -> { X = point2.X + (int i) * (point2.X - point1.X); Y = point2.Y + (int i) * (point2.Y - point1.Y) })
            |> Seq.takeWhile isPointInBounds

    seq {
        yield! antinodeList1
        yield! antinodeList2
        yield point1
        yield point2
    } |> List.ofSeq


let antinodes =
    antennaGroupPairings
        |> List.collect
            (fun (_, list) ->
                list
                    |> List.collect
                        (fun (antenna1, antenna2) ->
                            getAntinodes antenna1.Position antenna2.Position))
        |> List.distinct

printfn "%d" (antinodes.Length)
