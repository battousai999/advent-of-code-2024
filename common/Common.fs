module Common

open System
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic

let (|Regexer|_|) (regex : Regex) input =
    let m = regex.Match(input)

    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let buildMap<'a> (defaultElement: 'a) (projection: (char -> 'a)) (rawMap: string array) =
    let xSize = rawMap |> Array.head |> Seq.length
    let ySize = rawMap |> Array.length
    let map = Array2D.create xSize ySize defaultElement

    rawMap
        |> Array.iteri
            (fun yIndex line ->
                line
                    |> Seq.iteri
                        (fun xIndex ch ->
                            let value = projection ch
                            map[xIndex, yIndex] <- value))

    map

let renderMapi<'a> (projection: (int -> int -> 'a -> char)) (map: 'a array2d) =
    let sizeX = Array2D.length1 map
    let sizeY = Array2D.length2 map
    let rowIndices = [0..sizeY-1]
    let columnIndices = [0..sizeX-1]

    let output =
        let builder = StringBuilder()

        rowIndices
        |> List.iter
            (fun y ->
                columnIndices
                |> List.iter
                    (fun x ->
                        let ch = projection x y map[x,y]

                        builder.Append(ch) |> ignore)

                builder.AppendLine() |> ignore)

        builder.ToString()

    Console.WriteLine(output)

let renderMap<'a> (projection: ('a -> char)) (map: 'a array2d) =
    let sizeX = Array2D.length1 map
    let sizeY = Array2D.length2 map
    let rowIndices = [0..sizeY-1]
    let columnIndices = [0..sizeX-1]

    let output =
        let builder = StringBuilder()

        rowIndices
        |> List.iter
            (fun y ->
                columnIndices
                |> List.iter
                    (fun x ->
                        let ch = projection map[x,y]

                        builder.Append(ch) |> ignore)

                builder.AppendLine() |> ignore)

        builder.ToString()

    Console.WriteLine(output)

let identity x = x

let toSeq<'a> (arr: 'a array2d) =
    let length1 = arr |> Array2D.length1
    let length2 = arr |> Array2D.length2
    seq {
        for i in 0 .. length1-1 do
        for j in 0 .. length2-1 do
        yield arr[i,j]
    }

let toSeqOfIndices<'a> (arr: 'a array2d) =
    let length1 = arr |> Array2D.length1
    let length2 = arr |> Array2D.length2
    seq {
        for i in 0 .. length1-1 do
        for j in 0 .. length2-1 do
        yield (i,j)
    }

let findInMap<'a> (predicate: int -> int -> 'a -> bool) (map: 'a array2d) =
    map |> toSeqOfIndices |> Seq.find (fun (x, y) -> predicate x y map[x,y])

let infiniteSeq () =
    seq {
        let mutable i = 0L

        while true do
            if i < Int64.MaxValue then
                i <- i + 1L

            yield i
    }

let isEven n = n % 2 = 0
let isOdd n = n % 2 = 1

let toChars (str: string) =
    seq {
        for i in 0..str.Length-1 do
            yield str[i]
    }

let findPositionInMap<'a> predicate (map: 'a array2d) =
    map |> toSeqOfIndices |> Seq.tryPick (fun (x, y) -> if predicate map[x,y] then Some (x, y) else None)

type Vertex<'a when 'a: equality> = {
    Data: 'a
}

type Edge<'a when 'a: equality> = {
    Source: Vertex<'a>
    Dest: Vertex<'a>
    Weight: int
}

type Graph<'a when 'a: equality> = {
    Vertices: Vertex<'a> list
    Edges: Edge<'a> list
}

type DijkstraResults<'a when 'a: equality> = {
    DistanceMap: Dictionary<Vertex<'a>, int>
    PrevMap: Dictionary<Vertex<'a>, Vertex<'a>>
}

let getEdge<'a when 'a: equality> (source: Vertex<'a>) (dest: Vertex<'a>) (edges: Edge<'a> list) =
    edges |> List.find (fun edge -> edge.Source = source && edge.Dest = dest)

let getNeighbors<'a when 'a: equality> (vertex: Vertex<'a>) (edges: Edge<'a> list) =
    edges |> List.filter (fun v -> v.Source = vertex)

// function Dijkstra(Graph, source):
//     dist[source] ← 0                           // Initialization
//     create vertex priority queue Q
//     for each vertex v in Graph.Vertices:
//         if v ≠ source
//             dist[v] ← INFINITY                 // Unknown distance from source to v
//             prev[v] ← UNDEFINED                // Predecessor of v
//
//     Q.add_with_priority(source, dist[source])
//
//     while Q is not empty:                      // The main loop
//         u ← Q.extract_min()                    // Remove and return best vertex
//         if u = target:
//             break out of while loop
//         for each neighbor v of u:              // Go through all v neighbors of u
//             alt ← dist[u] + Graph.Edges(u, v)
//             if alt < dist[v]:
//                 dist[v] ← alt
//                 prev[v] ← u
//                 if not Q.contains(v):
//                     Q.add_with_priority(v, alt)
//     return dist, prev

// -- To get shortest path
// S ← empty sequence
// u ← target
// if prev[u] is defined or u = source:          // Do something only if the vertex is reachable
//     while u is defined:                       // Construct the shortest path with a stack S
//         insert u at the beginning of S        // Push the vertex onto the stack
//         u ← prev[u]                           // Traverse from target to source
//
// Dijkstra's shortest path algorithm.
// https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
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

            neighbors
            |> List.iter
                (fun edge ->
                    let projection = weightFunction |> Option.defaultValue (fun e dm _ -> dm[e.Source] + e.Weight)
                    let alternate = projection edge distanceMap prevMap // distanceMap[current] + edge.Weight

                    if alternate < distanceMap[edge.Dest] then
                        distanceMap[edge.Dest] <- alternate
                        prevMap[edge.Dest] <- current

                        let containsDest = queue.UnorderedItems |> Seq.exists (fun ((v, _): struct(Vertex<'a> * int)) -> v = edge.Dest)

                        if not containsDest then
                            queue.Enqueue(edge.Dest, alternate))

    { DistanceMap = distanceMap; PrevMap = prevMap }

let getShortestPath<'a when 'a: equality> (source: Vertex<'a>) (target: Vertex<'a>) (prevMap: Dictionary<Vertex<'a>, Vertex<'a>>) =
    let mutable path = []

    if prevMap.ContainsKey(target) || target = source then
        let mutable current = Some target

        while Option.isSome current do
            path <- (Option.get current) :: path
            current <- if prevMap.ContainsKey(Option.get current) then Some prevMap[Option.get current] else None

    path

// So, for [1; 2; 3; 4] returns [ [1]; [1; 2]; [1; 2; 3]; [1; 2; 3; 4] ]
let getListSequences list =
    list
    |> List.fold
        (fun (acc, prev) x ->
            let newSequence = prev @ [x]
            (acc @ [newSequence], newSequence)
            )
        ([], [])
    |> fst