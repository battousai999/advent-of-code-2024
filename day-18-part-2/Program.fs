// The Historians aren't as used to moving around in this pixelated universe as you are. You're afraid they're not going to
//  be fast enough to make it to the exit before the path is completely blocked.
//
// To determine how fast everyone needs to go, you need to determine the first byte that will cut off the path to the exit.
//
// In the above example, after the byte at 1,1 falls, there is still a path to the exit:
//
// O..#OOO
// O##OO#O
// O#OO#OO
// OOO#OO#
// ###OO##
// .##O###
// #.#OOOO
//
// However, after adding the very next byte (at 6,1), there is no longer a path to the exit:
//
// ...#...
// .##..##
// .#..#..
// ...#..#
// ###..##
// .##.###
// #.#....
//
// So, in this example, the coordinates of the first byte that prevents the exit from being reachable are 6,1.
//
// Simulate more of the bytes that are about to corrupt your memory space. What are the coordinates of the first byte that will
// prevent the exit from being reachable from your starting position? (Provide the answer as two integers separated by a comma
// with no other characters.)

open System
open System.IO
open System.Collections.Generic
open Common

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

type MapElement =
| Empty
| Corrupted

let gridSize = 71
let startingCorruptions = 1024
let rawPositions = File.ReadAllLines("../day-18-part-1/input.txt")

// let gridSize = 7
// let startingCorruptions = 12

// let rawPositionsStr = @"5,4
// 4,2
// 4,5
// 3,0
// 2,1
// 6,3
// 2,4
// 1,5
// 0,6
// 3,3
// 2,6
// 5,1
// 1,2
// 5,5
// 2,5
// 6,5
// 1,4
// 0,4
// 6,4
// 1,1
// 6,1
// 1,0
// 0,5
// 1,6
// 2,0"

// let rawPositions = rawPositionsStr.Split(Environment.NewLine)

let startingPosition = { X = 0; Y = 0 }
let endingPosition = { X = gridSize - 1; Y = gridSize - 1 }

let corruptionPositions =
    rawPositions
    |> Array.map
        (fun x ->
            match x.Split(",") with
            | [|x; y|] -> { X = int x; Y =  int y }
            | _ -> raise <| ApplicationException($"Invalid corruption position: {x}"))

let sequenceOfMapPoints () =
    seq {
        for i in 0 .. gridSize-1 do
        for j in 0 .. gridSize-1 do
        yield (i,j)
    }

let getPointInDirection position direction =
    match direction with
    | North when position.Y > 0            -> Some { X = position.X;     Y = position.Y - 1 }
    | East  when position.X < gridSize - 1 -> Some { X = position.X + 1; Y = position.Y     }
    | South when position.Y < gridSize - 1 -> Some { X = position.X;     Y = position.Y + 1 }
    | West  when position.X > 0            -> Some { X = position.X - 1; Y = position.Y     }
    | _ -> None

let buildGraph (corruptionPositions: HashSet<Point>) =
    let vertices = HashSet()
    let edges = ResizeArray()

    sequenceOfMapPoints ()
    |> Seq.iter
        (fun (x, y) ->
            let position = { X = x; Y = y }

            if not <| corruptionPositions.Contains(position) then
                let vertex = { Data = position }
                let neighbors =
                    directions
                    |> List.choose
                        (fun direction ->
                            getPointInDirection position direction
                            |> Option.filter (fun neighborPoint -> not <| corruptionPositions.Contains(neighborPoint)))

                vertices.Add(vertex) |> ignore

                neighbors
                |> List.iter
                    (fun neighbor ->
                        let neighborVertex = { Data = neighbor }

                        vertices.Add(neighborVertex) |> ignore
                        edges.Add({ Source = vertex; Dest = neighborVertex; Weight = 1 })))

    { Vertices = vertices |> Seq.toList; Edges = edges |> Seq.toList }

let graph = buildGraph (corruptionPositions |> Array.take startingCorruptions |> HashSet)

let startingVertex = graph.Vertices |> List.find (fun v -> v.Data = startingPosition)
let endingVertex = graph.Vertices |> List.find (fun v -> v.Data = endingPosition)

let removeVertexFromGraph vertex graph =
    {
        Vertices = graph.Vertices |> List.filter (fun v -> v <> vertex)
        Edges = graph.Edges |> List.filter (fun e -> e.Dest <> vertex && e.Source <> vertex)
    }

let findFirstFailingPoint startingGraph =
    let mutable hasFailed = false
    let mutable graph = startingGraph
    let mutable lastCorruptionPoint = None
    let queue = Queue(corruptionPositions |> Array.skip startingCorruptions)
    let firstResult = dijkstra graph startingVertex endingVertex None
    let mutable currentPath = getShortestPath startingVertex endingVertex firstResult.PrevMap

    while queue.Count > 0 && not hasFailed do
        let currentCorruptionPoint = queue.Dequeue()
        let currentCorruptionVertex = { Data = currentCorruptionPoint }
        lastCorruptionPoint <- Some currentCorruptionPoint

        graph <- removeVertexFromGraph currentCorruptionVertex graph

        if currentPath |> List.contains currentCorruptionVertex then
            let newResult = dijkstra graph startingVertex endingVertex None

            if not newResult.IsFound then
                hasFailed <- true
            else
                currentPath <- getShortestPath startingVertex endingVertex newResult.PrevMap

    if hasFailed then lastCorruptionPoint else None

let failedPoint = findFirstFailingPoint graph

if Option.isSome failedPoint then
    printfn "\n\nfailure point = %d,%d" (Option.get failedPoint).X (Option.get failedPoint).Y
else
    printfn "\n\nno failure point"
