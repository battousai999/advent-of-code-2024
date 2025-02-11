// The Historians are quite pixelated again. This time, a massive, black building looms over you - you're right
// outside the CPU!
//
// While The Historians get to work, a nearby program sees that you're idle and challenges you to a race. Apparently,
// you've arrived just in time for the frequently-held race condition festival!
//
// The race takes place on a particularly long and twisting code path; programs compete to see who can finish in the
// fewest picoseconds. The winner even gets their very own mutex!
//
// They hand you a map of the racetrack (your puzzle input). For example:
//
// ###############
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
// ###############
//
// The map consists of track (.) - including the start (S) and end (E) positions (both of which also count as track) - and
// walls (#).
//
// When a program runs through the racetrack, it starts at the start position. Then, it is allowed to move up, down, left,
// or right; each such move takes 1 picosecond. The goal is to reach the end position as quickly as possible. In this example
// racetrack, the fastest time is 84 picoseconds.
//
// Because there is only a single path from the start to the end and the programs all go the same speed, the races used to be
// pretty boring. To make things more interesting, they introduced a new rule to the races: programs are allowed to cheat.
//
// The rules for cheating are very strict. Exactly once during a race, a program may disable collision for up to 2 picoseconds.
// This allows the program to pass through walls as if they were regular track. At the end of the cheat, the program must be back
// on normal track again; otherwise, it will receive a segmentation fault and get disqualified.
//
// So, a program could complete the course in 72 picoseconds (saving 12 picoseconds) by cheating for the two moves marked 1 and 2:
//
// ###############
// #...#...12....#
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
// ###############
//
// Or, a program could complete the course in 64 picoseconds (saving 20 picoseconds) by cheating for the two moves marked 1 and 2:
//
// ###############
// #...#...#.....#
// #.#.#.#.#.###.#
// #S#...#.#.#...#
// #######.#.#.###
// #######.#.#...#
// #######.#.###.#
// ###..E#...12..#
// ###.#######.###
// #...###...#...#
// #.#####.#.###.#
// #.#...#.#.#...#
// #.#.#.#.#.#.###
// #...#...#...###
// ###############
//
// This cheat saves 38 picoseconds:
//
// ###############
// #...#...#.....#
// #.#.#.#.#.###.#
// #S#...#.#.#...#
// #######.#.#.###
// #######.#.#...#
// #######.#.###.#
// ###..E#...#...#
// ###.####1##.###
// #...###.2.#...#
// #.#####.#.###.#
// #.#...#.#.#...#
// #.#.#.#.#.#.###
// #...#...#...###
// ###############
//
// This cheat saves 64 picoseconds and takes the program directly to the end:
//
// ###############
// #...#...#.....#
// #.#.#.#.#.###.#
// #S#...#.#.#...#
// #######.#.#.###
// #######.#.#...#
// #######.#.###.#
// ###..21...#...#
// ###.#######.###
// #...###...#...#
// #.#####.#.###.#
// #.#...#.#.#...#
// #.#.#.#.#.#.###
// #...#...#...###
// ###############
//
// Each cheat has a distinct start position (the position where the cheat is activated, just before the first move that is allowed
// to go through walls) and end position; cheats are uniquely identified by their start position and end position.
//
// In this example, the total number of cheats (grouped by the amount of time they save) are as follows:
//
// * There are 14 cheats that save 2 picoseconds.
// * There are 14 cheats that save 4 picoseconds.
// * There are 2 cheats that save 6 picoseconds.
// * There are 4 cheats that save 8 picoseconds.
// * There are 2 cheats that save 10 picoseconds.
// * There are 3 cheats that save 12 picoseconds.
// * There is one cheat that saves 20 picoseconds.
// * There is one cheat that saves 36 picoseconds.
// * There is one cheat that saves 38 picoseconds.
// * There is one cheat that saves 40 picoseconds.
// * There is one cheat that saves 64 picoseconds.
//
// You aren't sure what the conditions of the racetrack will be like, so to give yourself as many options as possible, you'll need a
// list of the best cheats. How many cheats would save you at least 100 picoseconds?

open System
open System.IO
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

// let rawMap = File.ReadAllLines("./input.txt")

let rawMapStr = @"###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############"

let rawMap = rawMapStr.Split(Environment.NewLine)

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

map
|> renderMap
    (fun element ->
        match element with
        | Empty -> '.'
        | Wall  -> '#'
        | Start -> 'S'
        | End   -> 'E')

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

// printfn "\n\n%A" path

let findCheats graph map path =
    let originalPathLength = path |> List.length

    path
    |> List.take (originalPathLength - 1)
    |> List.collect
        (fun vertex ->
            let point = vertex.Data
            let possibleCheats =
                directions
                |> List.choose
                    (fun d ->
                        getElementInDirection map point d
                        |> Option.map (fun (p, element) -> (d, p, element)))
                |> List.filter (fun (_, _, element) -> element = Wall)
                |> List.choose
                    (fun (d, p, element) ->
                        getElementInDirection map p d
                        |> Option.map(fun (newPoint, newElement) -> (d, newPoint, newElement)))
                |> List.filter (fun (_, _, element) -> element = Empty)
                |> List.map
                    (fun (_, p, _) ->
                        point, p)

            possibleCheats
            |> List.map
                (fun (sourcePoint, destPoint) ->
                    let sourceVertex = graph.Vertices |> List.find (fun v -> v.Data = sourcePoint)
                    let destVertex = graph.Vertices |> List.find (fun v -> v.Data = destPoint)
                    let newGraph = { graph with Edges = { Source = sourceVertex; Dest = destVertex; Weight = 1 } :: graph.Edges }
                    let newDijkstraResult = dijkstra newGraph startVertex endVertex None
                    let newShortestPath = getShortestPath startVertex endVertex newDijkstraResult.PrevMap

                    (sourcePoint, destPoint, originalPathLength - (newShortestPath.Length)))
            |> List.filter (fun (_, _, savings) -> savings > 0))

let cheats = findCheats graph map path

printfn "%A" cheats
