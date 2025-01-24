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

// let rawMap = File.ReadAllLines("../day-16-part-1/input.txt")

let rawMapStr = @"#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################"

let rawMap = rawMapStr.Split(Environment.NewLine)

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

// printfn "\nstart: (%d,%d), end: (%d,%d)" startMapPosition.X startMapPosition.Y endMapPosition.X endMapPosition.Y

let startVertex = graph.Vertices |> List.find (fun v -> v.Data = startMapPosition)
let endVertex = graph.Vertices |> List.find (fun v -> v.Data = endMapPosition)


let modifiedDijkstra<'a when 'a: equality>
    (graph: Graph<'a>)
    (source: Vertex<'a>)
    (target: Vertex<'a>)
    (weightFunction: (Edge<'a> -> Dictionary<Vertex<'a>, int> -> Dictionary<Vertex<'a>, ResizeArray<Vertex<'a>>> -> int) option) =  // edge -> distanceMap -> prevMap -> calculatedWeightValue
    let distanceMap = Dictionary<Vertex<'a>, int>()
    let prevMap = Dictionary<Vertex<'a>, ResizeArray<Vertex<'a>>>()
    let queue = PriorityQueue<Vertex<'a> * Vertex<'a> option, int>()

    distanceMap[source] <- 0

    graph.Vertices
    |> List.filter (fun v -> v <> source)
    |> List.iter (fun v -> distanceMap[v] <- (Int32.MaxValue - 2000))

    queue.Enqueue((source, None), 0)

    let mutable foundDistance = None

    while queue.Count > 0 do
        let current, from = queue.Dequeue()
        let isFound = Option.isSome foundDistance

        if current = target then
            if not isFound then
                foundDistance <- Some distanceMap[current]
        elif not isFound || distanceMap[current] <= Option.get foundDistance then
            let neighbors = getNeighbors current graph.Edges |> List.filter (fun e -> from |> Option.map (fun x -> x <> e.Dest) |> Option.defaultValue true)

            // printfn ">>> current: %A, neighbors: %A" current neighbors

            neighbors
            |> List.iter
                (fun edge ->
                    let projection = weightFunction |> Option.defaultValue (fun e dm _ -> dm[e.Source] + e.Weight)
                    let alternate = projection edge distanceMap prevMap // distanceMap[current] + edge.Weight

                    printfn ">>> at (%A) from (%A) alternate: %d, distanceMap[edge.Dest]: %d" edge.Dest.Data edge.Source.Data alternate distanceMap[edge.Dest]

                    let oldDistance = distanceMap[edge.Dest]
                    let isLessOrEqual = alternate <= oldDistance + 1000
                    let isEqual = alternate = (oldDistance - 1000) || alternate = oldDistance || alternate = (oldDistance + 1000)

                    if isLessOrEqual then
                        distanceMap[edge.Dest] <- alternate

                        if isEqual && prevMap.ContainsKey(edge.Dest) then
                            printfn ">>> >>> equals at %A" edge.Dest
                            prevMap[edge.Dest].Add(current)
                        else
                            prevMap[edge.Dest] <- ResizeArray([current])

                        let containsDest = queue.UnorderedItems |> Seq.exists (fun (((v, _), _): struct((Vertex<'a> * Vertex<'a> option) * int)) -> v = edge.Dest)

                        if not containsDest then
                            queue.Enqueue((edge.Dest, Some current), alternate))

    prevMap

let getDirection a b =
    match a, b with
    | { X = x1; Y = y1 }, { X = x2; Y = y2 } when x1 = x2 && y1 = y2 - 1 -> North
    | { X = x1; Y = y1 }, { X = x2; Y = y2 } when x1 = x2 && y1 = y2 + 1 -> South
    | { X = x1; Y = y1 }, { X = x2; Y = y2 } when x1 = x2 - 1 && y1 = y2 -> East
    | { X = x1; Y = y1 }, { X = x2; Y = y2 } when x1 = x2 + 1 && y1 = y2 -> West
    | _ -> raise <| ApplicationException($"Non-adjacent points: ({a.X}, {a.Y}) and ({b.X}, {b.Y})")

let directionIsOpposite dir1 dir2 =
    match dir1, dir2 with
    | North, South
    | South, North
    | East, West
    | West, East -> true
    | _ -> false

let modifiedGetShortestPath<'a when 'a: equality> (source: Vertex<'a>) (target: Vertex<'a>) (leadingEdge: Edge<'a>) (prevMap: Dictionary<Vertex<'a>, ResizeArray<Vertex<'a>>>) =
    let mutable path = []

    if prevMap.ContainsKey(target) || target = source then
        let mutable current = Some target

        while Option.isSome current do
            path <- (Option.get current) :: path

            let newCurrent =
                if prevMap.ContainsKey(Option.get current) then
                    let prevs = prevMap[Option.get current]
                    let prev =
                        prevs
                        |> Seq.tryFind (fun v -> Option.get current = leadingEdge.Dest && v = leadingEdge.Source)
                        |> Option.defaultValue prevs[0]
                    Some prev
                else
                    None

            current <- newCurrent

    path

let getAllShortestPathVertices<'a when 'a: equality> (source: Vertex<'a>) (target: Vertex<'a>) (prevMap: Dictionary<Vertex<'a>, ResizeArray<Vertex<'a>>>) =
    let mutable vertices = HashSet()

    if prevMap.ContainsKey(target) || target = source then
        let todo = HashSet([target])

        while todo.Count > 0 do
            let current = todo |> Seq.head

            todo.Remove(current) |> ignore
            vertices.Add(current) |> ignore

            if prevMap.ContainsKey(current) then
                // printfn ">>> %d" prevMap[current].Count

                prevMap[current] |> Seq.iter (fun v -> todo.Add(v) |> ignore)

    vertices |> List.ofSeq

let weightFunction edge prevMap =
    let path =
        modifiedGetShortestPath startVertex edge.Source edge prevMap
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
        |> List.map (fun (a, b) -> if a = b then 0 elif directionIsOpposite a b then 10000 else 1)
        |> List.sum

    (path.Length - 1) + (numDirectionChanges * 1000)

let results = modifiedDijkstra graph startVertex endVertex (Some (fun e _ pm -> weightFunction e pm))

// printfn "\n\nresults:\n%A" (Seq.toList results.DistanceMap |> List.filter (fun x -> x.Value < Int32.MaxValue))
// printfn "\n\npath:\n%A" (getShortestPath startVertex endVertex results.PrevMap)

let caluculateCost (graph: Graph<Point>) (endVertex: Vertex<Point>) (prevMap: Dictionary<Vertex<Point>,ResizeArray<Vertex<Point>>>) =
    let prevForEndVertex = prevMap[endVertex].[0]
    let edge = graph.Edges |> List.find (fun edge -> edge.Source = prevForEndVertex && edge.Dest = endVertex)

    weightFunction edge prevMap

printfn "\n\ncost = %d" (caluculateCost graph endVertex results)

renderMap map (getAllShortestPathVertices startVertex endVertex results)
