// Now that you know what the best paths look like, you can figure out the best spot to sit.
//
// Every non-wall tile (S, ., or E) is equipped with places to sit along the edges of the tile. While determining which
// of these tiles would be the best spot to sit depends on a whole bunch of factors (how comfortable the seats are, how
// far away the bathrooms are, whether there's a pillar blocking your view, etc.), the most important factor is whether
// the tile is on one of the best paths through the maze. If you sit somewhere else, you'd miss all the action!
//
// So, you'll need to determine which tiles are part of any best path through the maze, including the S and E tiles.
//
// In the first example, there are 45 tiles (marked O) that are part of at least one of the various best paths through the maze:
//
// ###############
// #.......#....O#
// #.#.###.#.###O#
// #.....#.#...#O#
// #.###.#####.#O#
// #.#.#.......#O#
// #.#.#####.###O#
// #..OOOOOOOOO#O#
// ###O#O#####O#O#
// #OOO#O....#O#O#
// #O#O#O###.#O#O#
// #OOOOO#...#O#O#
// #O###.#.#.#O#O#
// #O..#.....#OOO#
// ###############
//
// In the second example, there are 64 tiles that are part of at least one of the best paths:
//
// #################
// #...#...#...#..O#
// #.#.#.#.#.#.#.#O#
// #.#.#.#...#...#O#
// #.#.#.#.###.#.#O#
// #OOO#.#.#.....#O#
// #O#O#.#.#.#####O#
// #O#O..#.#.#OOOOO#
// #O#O#####.#O###O#
// #O#O#..OOOOO#OOO#
// #O#O###O#####O###
// #O#O#OOO#..OOO#.#
// #O#O#O#####O###.#
// #O#O#OOOOOOO..#.#
// #O#O#O#########.#
// #O#OOO..........#
// #################
//
// Analyze your map further. How many tiles are part of at least one of the best paths through the maze?

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

let rawMap = File.ReadAllLines("../day-16-part-1/input.txt")

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
            | Empty -> if path |> List.exists (fun v -> v.Data = { X = x; Y = y }) then 'O' else '.'
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

let startVertex = graph.Vertices |> List.find (fun v -> v.Data = startMapPosition)
let endVertex = graph.Vertices |> List.find (fun v -> v.Data = endMapPosition)

type DirectedVertex = {
    Vertex: Vertex<Point>
    Direction: Direction
}

type State = {
    Cost: int
    DirectedVertex: DirectedVertex
}

let getOppositeDirection direction =
    match direction with
    | North -> South
    | South -> North
    | East  -> West
    | West  -> East

let getPointInDirection direction position =
    match direction with
    | North -> { X = position.X; Y = position.Y - 1 }
    | South -> { X = position.X; Y = position.Y + 1 }
    | East  -> { X = position.X + 1; Y = position.Y }
    | West  -> { X = position.X - 1; Y = position.Y }

let getEdgeDirection edge =

    match edge.Source.Data, edge.Dest.Data with
    | { X = x1; Y = y1 }, { X = x2; Y = y2} when x1 = x2 && y1 = y2 + 1 -> North
    | { X = x1; Y = y1 }, { X = x2; Y = y2} when x1 = x2 && y1 = y2 - 1 -> South
    | { X = x1; Y = y1 }, { X = x2; Y = y2} when x1 = x2 - 1 && y1 = y2 -> East
    | { X = x1; Y = y1 }, { X = x2; Y = y2} when x1 = x2 + 1 && y1 = y2 -> West
    | _ -> raise <| ApplicationException($"Invalid non-adjacent edge: ({edge.Source.Data.X}, {edge.Source.Data.Y}), ({edge.Dest.Data.X}, {edge.Dest.Data.Y})")

// Modified from Common.dijkstra to keep multiple, same-cost paths in the prevMap
let dijkstra
    (graph: Graph<Point>)
    (source: Vertex<Point>)
    (target: Vertex<Point>) =
    let distanceMap = Dictionary<DirectedVertex, int>()
    let prevMap = Dictionary<DirectedVertex, ResizeArray<DirectedVertex>>()
    let queue = PriorityQueue<State, int>()
    let finalStates = HashSet()
    let getDistance dv = if distanceMap.ContainsKey(dv) then distanceMap[dv] else Int32.MaxValue

    distanceMap[{ Vertex = source; Direction = East }] <- 0

    queue.Enqueue({ Cost = 0; DirectedVertex = { Vertex = source; Direction = East } }, 0)

    let mutable lowestCost = Int32.MaxValue
    let mutable isDone = false

    while queue.Count > 0 && not isDone do
        let state = queue.Dequeue()

        if state.Cost <= getDistance state.DirectedVertex then
            let isAtEnd = state.DirectedVertex.Vertex = target

            if isAtEnd && state.Cost <= lowestCost then
                lowestCost <- state.Cost
                finalStates.Add(state.DirectedVertex) |> ignore
            elif not isAtEnd then
                let neighbors =
                    getNeighbors state.DirectedVertex.Vertex graph.Edges
                    |> List.filter
                        (fun e ->
                            let oppositeDirection = getOppositeDirection state.DirectedVertex.Direction
                            let fromPoint = getPointInDirection oppositeDirection state.DirectedVertex.Vertex.Data

                            e.Dest.Data <> fromPoint)

                neighbors
                |> List.iter
                    (fun edge ->
                        let edgeDirection = getEdgeDirection edge
                        let newDirectedVertex = { Vertex = edge.Dest; Direction = edgeDirection }
                        let newCost = state.Cost + (if edgeDirection = state.DirectedVertex.Direction then 1 else 1001)
                        let lowestDistance = getDistance newDirectedVertex

                        if newCost <= lowestDistance then
                            if newCost < lowestDistance then
                                prevMap[newDirectedVertex] <- ResizeArray([state.DirectedVertex])
                                distanceMap[newDirectedVertex] <-newCost
                            elif newCost = lowestDistance then
                                prevMap[newDirectedVertex].Add(state.DirectedVertex)

                            queue.Enqueue({ Cost = newCost; DirectedVertex = newDirectedVertex }, newCost))

    let newPrevMap = Dictionary(prevMap |> Seq.map (fun kvp -> KeyValuePair(kvp.Key, kvp.Value |> Seq.toList)))

    (finalStates, newPrevMap, lowestCost)

let (finalStates, prevMap, _) = dijkstra graph startVertex endVertex

let calculateTotalPositionsInPaths (finalStates: HashSet<DirectedVertex>) (prevMap: Dictionary<DirectedVertex,list<DirectedVertex>>) =
    let positionsInPaths = HashSet(finalStates)
    let states = Queue(finalStates)

    while states.Count > 0 do
        let vertex = states.Dequeue()
        let prevVertices = if prevMap.ContainsKey(vertex) then prevMap[vertex] else []

        prevVertices
        |> List.iter
            (fun prevVertex ->
                if not <| positionsInPaths.Contains(prevVertex) then
                    positionsInPaths.Add(prevVertex) |> ignore
                    states.Enqueue(prevVertex))

    let branches =
        positionsInPaths
        |> Seq.map _.Vertex
        |> Seq.countBy identity
        |> Seq.filter (fun (_, length) -> length > 1)
        |> Seq.length

    positionsInPaths.Count - branches

printfn "\n\npositions = %d" (calculateTotalPositionsInPaths finalStates prevMap)
