// The Historians use their fancy device again, this time to whisk you all away to the North Pole prototype suit manufacturing lab... in the year 1518! It turns out that having direct access to history is very convenient for a group of historians.
//
// You still have to be careful of time paradoxes, and so it will be important to avoid anyone from 1518 while The Historians search for the Chief. Unfortunately, a single guard is patrolling this part of the lab.
//
// Maybe you can work out where the guard will go ahead of time so that The Historians can search safely?
//
// You start by making a map (your puzzle input) of the situation. For example:
//
// ....#.....
// .........#
// ..........
// ..#.......
// .......#..
// ..........
// .#..^.....
// ........#.
// #.........
// ......#...
//
// The map shows the current position of the guard with ^ (to indicate the guard is currently facing up from the perspective of the map). Any obstructions - crates, desks, alchemical reactors, etc. - are shown as #.
//
// Lab guards in 1518 follow a very strict patrol protocol which involves repeatedly following these steps:
//
//     If there is something directly in front of you, turn right 90 degrees.
//     Otherwise, take a step forward.
//
// Following the above protocol, the guard moves up several times until she reaches an obstacle (in this case, a pile of failed suit prototypes):
//
// ....#.....
// ....^....#
// ..........
// ..#.......
// .......#..
// ..........
// .#........
// ........#.
// #.........
// ......#...
//
// Because there is now an obstacle in front of the guard, she turns right before continuing straight in her new facing direction:
//
// ....#.....
// ........>#
// ..........
// ..#.......
// .......#..
// ..........
// .#........
// ........#.
// #.........
// ......#...
//
// Reaching another obstacle (a spool of several very long polymers), she turns right again and continues downward:
//
// ....#.....
// .........#
// ..........
// ..#.......
// .......#..
// ..........
// .#......v.
// ........#.
// #.........
// ......#...
//
// This process continues for a while, but the guard eventually leaves the mapped area (after walking past a tank of universal solvent):
//
// ....#.....
// .........#
// ..........
// ..#.......
// .......#..
// ..........
// .#........
// ........#.
// #.........
// ......#v..
//
// By predicting the guard's route, you can determine which specific positions in the lab will be in the patrol path. Including the guard's starting position, the positions visited by the guard before leaving the area are marked with an X:
//
// ....#.....
// ....XXXXX#
// ....X...X.
// ..#.X...X.
// ..XXXXX#X.
// ..X.X.X.X.
// .#XXXXXXX.
// .XXXXXXX#.
// #XXXXXXX..
// ......#X..
//
// In this example, the guard will visit 41 distinct positions on your map.
//
// Predict the path of the guard. How many distinct positions will the guard visit before leaving the mapped area?

open System
open System.IO
open System.Linq

type Point = {
    X: int
    Y: int
}

type Direction =
    | North
    | East
    | South
    | West

type Entity =
    | EmptySpace
    | Obstacle
    | Travelled
    | Guard of Direction option

let rawMap = File.ReadAllLines "./input.txt"

// let rawMapStr = @"....#.....
// .........#
// ..........
// ..#.......
// .......#..
// ..........
// .#..^.....
// ........#.
// #.........
// ......#..."

// let rawMap = rawMapStr.Split(Environment.NewLine)

let sizeX = rawMap |> Array.head |> Seq.length
let sizeY = rawMap |> Array.length
let map = Array2D.create sizeX sizeY EmptySpace
let mutable guardPosition = (Guard (Some North), { X = 0; Y = 0 })

rawMap
    |> Array.iteri
        (fun yIndex line ->
            line
                |> Seq.iteri
                    (fun xIndex ch ->
                        let entity =
                            match ch with
                            | '.' -> EmptySpace
                            | '#' -> Obstacle
                            | '^' ->
                                guardPosition <- (Guard (Some North), { X = xIndex; Y = yIndex })
                                Guard (Some North)
                            | _ -> raise <| ApplicationException($"Unexpected char: {ch}")

                        map[xIndex, yIndex] <- entity))

let tick () =
    let (guard, guardPoint) = guardPosition

    match guard with
    | Guard (Some North) ->
        let newY = guardPoint.Y - 1

        if newY = -1 then
            map[guardPoint.X, guardPoint.Y] <- Travelled
            guardPosition <- (Guard None, guardPoint)
        else
            let newEntity = map[guardPoint.X, newY]

            if newEntity = Obstacle then
                guardPosition <- (Guard (Some East), guardPoint)
            else
                map[guardPoint.X, guardPoint.Y] <- Travelled
                map[guardPoint.X, newY] <- guard
                guardPosition <- (guard, { X = guardPoint.X; Y = newY })
    | Guard (Some East) ->
        let newX = guardPoint.X + 1

        if newX = sizeX then
            map[guardPoint.X, guardPoint.Y] <- Travelled
            guardPosition <- (Guard None, guardPoint)
        else
            let newEntity = map[newX, guardPoint.Y]

            if newEntity = Obstacle then
                guardPosition <- (Guard (Some South), guardPoint)
            else
                map[guardPoint.X, guardPoint.Y] <- Travelled
                map[newX, guardPoint.Y] <- guard
                guardPosition <- (guard, { X = newX; Y = guardPoint.Y })
    | Guard (Some South) ->
        let newY = guardPoint.Y + 1

        if newY = sizeY then
            map[guardPoint.X, guardPoint.Y] <- Travelled
            guardPosition <- (Guard None, guardPoint)
        else
            let newEntity = map[guardPoint.X, newY]

            if newEntity = Obstacle then
                guardPosition <- (Guard (Some West), guardPoint)
            else
                map[guardPoint.X, guardPoint.Y] <- Travelled
                map[guardPoint.X, newY] <- guard
                guardPosition <- (guard, { X = guardPoint.X; Y = newY })
    | Guard (Some West) ->
        let newX = guardPoint.X - 1

        if newX = -1 then
            map[guardPoint.X, guardPoint.Y] <- Travelled
            guardPosition <- (Guard None, guardPoint)
        else
            let newEntity = map[newX, guardPoint.Y]

            if newEntity = Obstacle then
                guardPosition <- (Guard (Some North), guardPoint)
            else
                map[guardPoint.X, guardPoint.Y] <- Travelled
                map[newX, guardPoint.Y] <- guard
                guardPosition <- (guard, { X = newX; Y = guardPoint.Y })
    | _ -> ()

let infiniteSeq () =
    let rec innerSeq n =
        seq {
            yield n
            yield! innerSeq (n + 1L)
        }

    innerSeq 1L

let numSteps =
    infiniteSeq()
        |> Seq.takeWhile
            (fun i ->
                tick()
                (fst guardPosition) <> Guard None)
        |> Seq.length

let numStuff =
    map
        |> Seq.cast<Entity>
        |> Seq.filter (fun entity -> entity = Travelled)
        |> Seq.length

printfn "%d" numStuff
