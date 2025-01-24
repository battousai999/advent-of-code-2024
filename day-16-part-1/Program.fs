// It's time again for the Reindeer Olympics! This year, the big event is the Reindeer Maze, where the Reindeer
// compete for the lowest score.
//
// You and The Historians arrive to search for the Chief right as the event is about to start. It wouldn't hurt
// to watch a little, right?
//
// The Reindeer start on the Start Tile (marked S) facing East and need to reach the End Tile (marked E). They
// can move forward one tile at a time (increasing their score by 1 point), but never into a wall (#). They can
// also rotate clockwise or counterclockwise 90 degrees at a time (increasing their score by 1000 points).
//
// To figure out the best place to sit, you start by grabbing a map (your puzzle input) from a nearby kiosk. For example:
//
// ###############
// #.......#....E#
// #.#.###.#.###.#
// #.....#.#...#.#
// #.###.#####.#.#
// #.#.#.......#.#
// #.#.#####.###.#
// #...........#.#
// ###.#.#####.#.#
// #...#.....#.#.#
// #.#.#.###.#.#.#
// #.....#...#.#.#
// #.###.#.#.#.#.#
// #S..#.....#...#
// ###############
//
// There are many paths through this maze, but taking any of the best paths would incur a score of only 7036. This can be
// achieved by taking a total of 36 steps forward and turning 90 degrees a total of 7 times:
//
//
// ###############
// #.......#....E#
// #.#.###.#.###^#
// #.....#.#...#^#
// #.###.#####.#^#
// #.#.#.......#^#
// #.#.#####.###^#
// #..>>>>>>>>v#^#
// ###^#.#####v#^#
// #>>^#.....#v#^#
// #^#.#.###.#v#^#
// #^....#...#v#^#
// #^###.#.#.#v#^#
// #S..#.....#>>^#
// ###############
//
// Here's a second example:
//
// #################
// #...#...#...#..E#
// #.#.#.#.#.#.#.#.#
// #.#.#.#...#...#.#
// #.#.#.#.###.#.#.#
// #...#.#.#.....#.#
// #.#.#.#.#.#####.#
// #.#...#.#.#.....#
// #.#.#####.#.###.#
// #.#.#.......#...#
// #.#.###.#####.###
// #.#.#...#.....#.#
// #.#.#.#####.###.#
// #.#.#.........#.#
// #.#.#.#########.#
// #S#.............#
// #################
//
// In this maze, the best paths cost 11048 points; following one such path would look like this:
//
// #################
// #...#...#...#..E#
// #.#.#.#.#.#.#.#^#
// #.#.#.#...#...#^#
// #.#.#.#.###.#.#^#
// #>>v#.#.#.....#^#
// #^#v#.#.#.#####^#
// #^#v..#.#.#>>>>^#
// #^#v#####.#^###.#
// #^#v#..>>>>^#...#
// #^#v###^#####.###
// #^#v#>>^#.....#.#
// #^#v#^#####.###.#
// #^#v#^........#.#
// #^#v#^#########.#
// #S#>>^..........#
// #################
//
// Note that the path shown above includes one 90 degree turn as the very first move, rotating the Reindeer from facing
// East to facing North.
//
// Analyze your map carefully. What is the lowest score a Reindeer could possibly get?

open System
open System.IO
open System.Collections.Generic
open Common

type MapElement =
| Wall
| Empty
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

type Step = {
    Position: Point
    Direction: Direction
}

let rawMap = File.ReadAllLines("./input.txt")

// let rawMapStr = @"#################
// #...#...#...#..E#
// #.#.#.#.#.#.#.#.#
// #.#.#.#...#...#.#
// #.#.#.#.###.#.#.#
// #...#.#.#.....#.#
// #.#.#.#.#.#####.#
// #.#...#.#.#.....#
// #.#.#####.#.###.#
// #.#.#.......#...#
// #.#.###.#####.###
// #.#.#...#.....#.#
// #.#.#.#####.###.#
// #.#.#.........#.#
// #.#.#.#########.#
// #S#.............#
// #################"

// let rawMap = rawMapStr.Split(Environment.NewLine)

let map =
    buildMap
        Empty
        (fun ch ->
            match ch with
            | '#' -> Wall
            | '.' -> Empty
            | 'S' -> Start
            | 'E' -> End
            | _ -> raise <| ApplicationException($"Unexpected character in map: {ch}"))
        rawMap

let renderMap  map  path =
    map
    |> Common.renderMapi
        (fun x y element ->
            match element with
            | Wall  -> '#'
            | Empty ->
                let direction = path |> List.tryPick (fun step -> if step.Position.X = x && step.Position.Y = y then Some step.Direction else None)

                match direction with
                | Some North -> '^'
                | Some East  -> '>'
                | Some South -> 'v'
                | Some West  -> '<'
                | None       -> '.'
            | Start -> 'S'
            | End   -> 'E')

renderMap map []

let startMapPosition = map |> findInMap (fun _ _ element -> element = Start) ||> (fun x y -> { X = x; Y = y })
let endMapPosition = map |> findInMap (fun _ _ element -> element = End) ||> (fun x y -> { X = x; Y = y })

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

// printfn "\nstart: (%d,%d), end: (%d,%d)" startMapPosition.X startMapPosition.Y endMapPosition.X endMapPosition.Y

let startVertex = graph.Vertices |> List.find (fun v -> v.Data = startMapPosition)
let endVertex = graph.Vertices |> List.find (fun v -> v.Data = endMapPosition)


let dijkstra<'a when 'a: equality>
    (graph: Graph<'a>)
    (source: Vertex<'a>)
    (target: Vertex<'a>)
    (weightFunction: (Edge<'a> -> Dictionary<Vertex<'a>, int> -> Dictionary<Vertex<'a>, Vertex<'a>> -> int) option) =  // edge -> distanceMap -> prevMap -> calculatedWeightValue
    let distanceMap = Dictionary<Vertex<'a>, int>()
    let prevMap = Dictionary<Vertex<'a>, Vertex<'a>>()
    let queue = PriorityQueue<Vertex<'a>, int>()

    distanceMap[source] <- 0

    graph.Vertices
    |> List.filter (fun v -> v <> source)
    |> List.iter (fun v -> distanceMap[v] <- Int32.MaxValue)

    queue.Enqueue(source, 0)

    let mutable isFound = false

    while queue.Count > 0 && not isFound do
        let current = queue.Dequeue()

        if current = target then
            isFound <- true
        else
            let neighbors = getNeighbors current graph.Edges

            // printfn ">>> current: %A, neighbors: %A" current neighbors

            neighbors
            |> List.iter
                (fun edge ->
                    let projection = weightFunction |> Option.defaultValue (fun e dm _ -> dm[e.Source] + e.Weight)
                    let alternate = projection edge distanceMap prevMap // distanceMap[current] + edge.Weight

                    // printfn ">>> >>> alternate: %d, distanceMap[edge.Dest]: %d" alternate distanceMap[edge.Dest]

                    if alternate < distanceMap[edge.Dest] then
                        distanceMap[edge.Dest] <- alternate
                        prevMap[edge.Dest] <- current

                        let containsDest = queue.UnorderedItems |> Seq.exists (fun ((v, _): struct(Vertex<'a> * int)) -> v = edge.Dest)

                        if not containsDest then
                            queue.Enqueue(edge.Dest, alternate))

    { DistanceMap = distanceMap; PrevMap = prevMap }

let getDirection a b =
    match a, b with
    | { X = x1; Y = y1 }, { X = x2; Y = y2 } when x1 = x2 && y1 = y2 - 1 -> North
    | { X = x1; Y = y1 }, { X = x2; Y = y2 } when x1 = x2 && y1 = y2 + 1 -> South
    | { X = x1; Y = y1 }, { X = x2; Y = y2 } when x1 = x2 - 1 && y1 = y2 -> East
    | { X = x1; Y = y1 }, { X = x2; Y = y2 } when x1 = x2 + 1 && y1 = y2 -> West
    | _ -> raise <| ApplicationException($"Non-adjacent points: ({a.X}, {a.Y}) and ({b.X}, {b.Y})")

let weightFunction edge prevMap =
    let path =
        getShortestPath startVertex edge.Source prevMap
        |> (fun l -> List.append l [edge.Dest])
        |> List.map _.Data

    // printfn ">>> path = %A" path

    let directionPath =
        path
        |> List.pairwise
        |> List.map (fun x -> x ||> getDirection)
        |> List.append [East]

    let numDirectionChanges =
        directionPath
        |> List.pairwise
        |> List.map (fun (a, b) -> if a = b then 0 else 1)
        |> List.sum

    (path.Length - 1) + (numDirectionChanges * 1000)

let results = dijkstra graph startVertex endVertex (Some (fun e _ pm -> weightFunction e pm))

// printfn "\n\nresults:\n%A" (Seq.toList results.DistanceMap |> List.filter (fun x -> x.Value < Int32.MaxValue))
// printfn "\n\npath:\n%A" (getShortestPath startVertex endVertex results.PrevMap)

let caluculateCost (graph: Graph<Point>) (endVertex: Vertex<Point>) (prevMap: Dictionary<Vertex<Point>,Vertex<Point>>) =
    let prevForEndVertex = prevMap[endVertex]
    let edge = graph.Edges |> List.find (fun edge -> edge.Source = prevForEndVertex && edge.Dest = endVertex)

    weightFunction edge prevMap

printfn "\n\ncost = %d" (caluculateCost graph endVertex results.PrevMap)
