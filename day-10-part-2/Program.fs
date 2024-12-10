// The reindeer spends a few minutes reviewing your hiking trail map before realizing something, disappearing for a few minutes, and finally returning with yet another slightly-charred piece of paper.
//
// The paper describes a second way to measure a trailhead called its rating. A trailhead's rating is the number of distinct hiking trails which begin at that trailhead. For example:
//
// .....0.
// ..4321.
// ..5..2.
// ..6543.
// ..7..4.
// ..8765.
// ..9....
//
// The above map has a single trailhead; its rating is 3 because there are exactly three distinct hiking trails which begin at that position:
//
// .....0.   .....0.   .....0.
// ..4321.   .....1.   .....1.
// ..5....   .....2.   .....2.
// ..6....   ..6543.   .....3.
// ..7....   ..7....   .....4.
// ..8....   ..8....   ..8765.
// ..9....   ..9....   ..9....
//
// Here is a map containing a single trailhead with rating 13:
//
// ..90..9
// ...1.98
// ...2..7
// 6543456
// 765.987
// 876....
// 987....
//
// This map contains a single trailhead with rating 227 (because there are 121 distinct hiking trails that lead to the 9 on the right edge and 106 that lead to the 9 on the bottom edge):
//
// 012345
// 123456
// 234567
// 345678
// 4.6789
// 56789.
//
// Here's the larger example from before:
//
// 89010123
// 78121874
// 87430965
// 96549874
// 45678903
// 32019012
// 01329801
// 10456732
//
// Considering its trailheads in reading order, they have ratings of 20, 24, 10, 4, 1, 4, 5, 8, and 5. The sum of all trailhead ratings in this larger example topographic map is 81.
//
// You're not sure how, but the reindeer seems to have crafted some tiny flags out of toothpicks and bits of paper and is using them to mark trailheads on your topographic map. What is the sum of the ratings of all trailheads?

open System
open System.IO

type Point = {
    X: int
    Y: int
}

type Direction =
    | North
    | East
    | South
    | West

let allDirections = [ North; East; South; West ]

let allDirectionsExcept direction =
    match direction with
    | Some dir -> allDirections |> List.filter (fun x -> x <> dir)
    | None -> allDirections

let getOppositeDirection direction =
    match direction with
    | North -> South
    | East  -> West
    | South -> North
    | West  -> East

let rawMap = File.ReadAllLines "../day-10-part-1/input.txt"

// let rawMapStr = @"89010123
// 78121874
// 87430965
// 96549874
// 45678903
// 32019012
// 01329801
// 10456732"

// let rawMap = rawMapStr.Split(Environment.NewLine)

let sizeX = rawMap |> Array.head |> Seq.length
let sizeY  = rawMap |> Array.length

let map = Array2D.create sizeX sizeY 0

rawMap
    |> Array.iteri
        (fun yIndex line ->
            line
                |> Seq.iteri
                    (fun xIndex ch -> map[xIndex, yIndex] <- (string ch) |> Convert.ToInt32))

let roots =
    map
        |> Array2D.mapi
            (fun x y value -> (x, y, value))
        |> Seq.cast<int * int * int>
        |> Seq.filter (fun (_, _, value) -> value = 0)
        |> Seq.map (fun (x, y, _) -> { X = x; Y = y })
        |> List.ofSeq

let isInBounds point = (point.X >= 0 && point.X < sizeX) && (point.Y >= 0 && point.Y < sizeY)

let getPositionInDirection point direction =
    let newPoint =
        match direction with
        | North -> { X = point.X;     Y = point.Y - 1 }
        | East  -> { X = point.X + 1; Y = point.Y }
        | South -> { X = point.X;     Y = point.Y + 1 }
        | West  -> { X = point.X - 1; Y = point.Y }

    if isInBounds newPoint then Some newPoint else None

let findPathEndings startingPoint =
    let rec innerFindPathEndings point value fromDirection =
        if value = 9 then
            [point]
        else
            let directions = allDirectionsExcept fromDirection
            let checkDirection direction =
                let directionPosition = getPositionInDirection point direction

                directionPosition
                    |> Option.filter (fun p -> map[p.X, p.Y] = value + 1)
                    |> Option.map (fun p -> innerFindPathEndings p (value + 1) (Some (getOppositeDirection direction)))
                    |> Option.defaultValue []

            directions |> List.collect checkDirection

    innerFindPathEndings startingPoint 0 None

let reachableEndpoints = roots |> List.map (fun point -> (point, findPathEndings point |> List.length))

let results = reachableEndpoints |> List.sumBy snd

printfn "%d" results
