// The programs seem perplexed by your list of cheats. Apparently, the two-picosecond cheating rule was deprecated several
// milliseconds ago! The latest version of the cheating rule permits a single cheat that instead lasts at most 20 picoseconds.
//
// Now, in addition to all the cheats that were possible in just two picoseconds, many more cheats are possible. This
// six-picosecond cheat saves 76 picoseconds:
//
// ###############
// #...#...#.....#
// #.#.#.#.#.###.#
// #S#...#.#.#...#
// #1#####.#.#.###
// #2#####.#.#...#
// #3#####.#.###.#
// #456.E#...#...#
// ###.#######.###
// #...###...#...#
// #.#####.#.###.#
// #.#...#.#.#...#
// #.#.#.#.#.#.###
// #...#...#...###
// ###############
//
// Because this cheat has the same start and end positions as the one above, it's the same cheat, even though the path taken
// during the cheat is different:
//
// ###############
// #...#...#.....#
// #.#.#.#.#.###.#
// #S12..#.#.#...#
// ###3###.#.#.###
// ###4###.#.#...#
// ###5###.#.###.#
// ###6.E#...#...#
// ###.#######.###
// #...###...#...#
// #.#####.#.###.#
// #.#...#.#.#...#
// #.#.#.#.#.#.###
// #...#...#...###
// ###############
//
// Cheats don't need to use all 20 picoseconds; cheats can last any amount of time up to and including 20 picoseconds (but can
// still only end when the program is on normal track). Any cheat time not used is lost; it can't be saved for another cheat later.
//
// You'll still need a list of the best cheats, but now there are even more to choose between. Here are the quantities of cheats
// in this example that save 50 picoseconds or more:
//
// - There are 32 cheats that save 50 picoseconds.
// - There are 31 cheats that save 52 picoseconds.
// - There are 29 cheats that save 54 picoseconds.
// - There are 39 cheats that save 56 picoseconds.
// - There are 25 cheats that save 58 picoseconds.
// - There are 23 cheats that save 60 picoseconds.
// - There are 20 cheats that save 62 picoseconds.
// - There are 19 cheats that save 64 picoseconds.
// - There are 12 cheats that save 66 picoseconds.
// - There are 14 cheats that save 68 picoseconds.
// - There are 12 cheats that save 70 picoseconds.
// - There are 22 cheats that save 72 picoseconds.
// - There are 4 cheats that save 74 picoseconds.
// - There are 3 cheats that save 76 picoseconds.
//
// Find the best cheats using the updated cheating rules. How many cheats would save you at least 100 picoseconds?

open System
open System.IO
open System.Linq
open Common
open System.Collections.Generic

type MapElement =
| Empty
| Wall
| Start
| End

type Point = {
    X: int
    Y: int
}

type Direction =
| North
| East
| South
| West

let directions = [North; East; South; West]

let rawMap = File.ReadAllLines("../day-20-part-1/input.txt")

// let rawMapStr = @"###############
// #...#...#.....#
// #.#.#.#.#.###.#
// #S#...#.#.#...#
// #######.#.#.###
// #######.#.#...#
// #######.#.###.#
// ###..E#...#...#
// ###.#######.###
// #...###...#...#
// #.#####.#.###.#
// #.#...#.#.#...#
// #.#.#.#.#.#.###
// #...#...#...###
// ###############"

// let rawMap = rawMapStr.Split(Environment.NewLine)

let map =
    let projection ch =
        match ch with
        | '.' -> Empty
        | '#' -> Wall
        | 'S' -> Start
        | 'E' -> End
        | _ -> raise <| ApplicationException($"Invalid map character: {ch}")

    rawMap
    |> buildMap Empty projection

let getElementInDirection map position direction =
    let sizeX = Array2D.length1 map
    let sizeY = Array2D.length2 map

    match direction with
    | North when position.Y > 0         -> Some ({ X = position.X;     Y = position.Y - 1 }, map[position.X,     position.Y - 1])
    | East  when position.X < sizeX - 1 -> Some ({ X = position.X + 1; Y = position.Y     }, map[position.X + 1, position.Y    ])
    | South when position.Y < sizeY - 1 -> Some ({ X = position.X;     Y = position.Y + 1 }, map[position.X,     position.Y + 1])
    | West  when position.X > 0         -> Some ({ X = position.X - 1; Y = position.Y     }, map[position.X - 1, position.Y    ])
    | _ -> None

let translateMapToGraph map =
    let vertices = HashSet()
    let edges = ResizeArray()

    map
    |> Array2D.iteri
        (fun x y element ->
            if element = Empty || element = Start || element = End then
                let position = { X = x; Y = y }
                let vertex = { Data = position }
                let neighbors =
                    directions
                    |> List.choose
                        (fun direction ->
                            getElementInDirection map position direction
                            |> Option.filter (fun (_, neighborElement) -> neighborElement = Empty || neighborElement = Start || neighborElement = End))

                vertices.Add(vertex) |> ignore

                neighbors
                |> List.iter
                    (fun (neighborPoint, _) ->
                        let neighborVertex = { Data = neighborPoint }

                        vertices.Add(neighborVertex) |> ignore
                        edges.Add({ Source = vertex; Dest = neighborVertex; Weight = 1 })))

    { Vertices = vertices |> Seq.toList; Edges = edges |> Seq.toList }

let graph = translateMapToGraph map

let startMapPosition = map |> findInMap (fun _ _ element -> element = Start) ||> (fun x y -> { X = x; Y = y })
let endMapPosition = map |> findInMap (fun _ _ element -> element = End) ||> (fun x y -> { X = x; Y = y })

let startVertex = graph.Vertices |> List.find (fun v -> v.Data = startMapPosition)
let endVertex = graph.Vertices |> List.find (fun v -> v.Data = endMapPosition)

let results = dijkstra graph startVertex endVertex None
let path = getShortestPath startVertex endVertex results.PrevMap

let pathRanks = (path |> List.indexed |> Seq.ofList).ToDictionary(snd >> _.Data, fst)

let savingsThreshold = 100

let getPointsInRange map distance startingPoint =
    let xSize = map |> Array2D.length1
    let ySize = map |> Array2D.length2
    let topPoints =
        seq {
            for j in startingPoint.Y - distance .. startingPoint.Y - 1 do
            for i in startingPoint.X - (j - (startingPoint.Y - distance)) .. startingPoint.X + (j - (startingPoint.Y - distance)) do
            yield { X = i; Y = j }
        }
    let middlePoints =
        seq {
            for i in startingPoint.X - distance .. startingPoint.X + distance do
            yield { X = i; Y = startingPoint.Y }
        }
    let bottomPoints =
        seq {
            for j in startingPoint.Y + 1 .. startingPoint.Y + distance do
            for i in startingPoint.X - (startingPoint.Y + distance - j) .. startingPoint.X + (startingPoint.Y + distance - j) do
            yield { X = i; Y = j }
        }

    seq {
        yield! topPoints
        yield! middlePoints
        yield! bottomPoints
    }
    |> Seq.filter
        (fun p ->
            let inXRange = p.X >= 0 && p.X < xSize
            let inYRange = p.Y >= 0 && p.Y < ySize

            inXRange && inYRange && p <> startingPoint)

let findCheats map path =
    let originalPathLength = path |> List.length

    path
    |> List.take (originalPathLength - 1)
    |> List.collect
        (fun vertex ->
            let sourcePoint = vertex.Data
            let possibleCheats =
                let possibleDestPoints =
                    getPointsInRange map 20 sourcePoint
                    |> List.filter
                        (fun p ->
                            let elementAtPoint = map[p.X, p.Y]

                            elementAtPoint = Empty || elementAtPoint = End)


// TODO: In findCheats, with all "points in range" (should look like a diamond), find points (which will be destPoints) that
// end on Empty or End and are ranked greater than the sourcePoint that generated the "points in range" (which will be
// done for each point on inital path).  With sourcePoint and destPoint, savings can be calculated with: pathRanks[destPoint] - pathRanks[sourcePoint] - 2
