// While The Historians begin working around the guard's patrol route, you borrow their fancy device and step outside the lab. From the safety of a supply closet, you time travel through the last few months and record the nightly status of the lab's guard post on the walls of the closet.
//
// Returning after what seems like only a few seconds to The Historians, they explain that the guard's patrol area is simply too large for them to safely search the lab without getting caught.
//
// Fortunately, they are pretty sure that adding a single new obstruction won't cause a time paradox. They'd like to place the new obstruction in such a way that the guard will get stuck in a loop, making the rest of the lab safe to search.
//
// To have the lowest chance of creating a time paradox, The Historians would like to know all of the possible positions for such an obstruction. The new obstruction can't be placed at the guard's starting position - the guard is there right now and would notice.
//
// In the above example, there are only 6 different positions where a new obstruction would cause the guard to get stuck in a loop. The diagrams of these six situations use O to mark the new obstruction, | to show a position where the guard moves up/down, - to show a position where the guard moves left/right, and + to show a position where the guard moves both up/down and left/right.
//
// Option one, put a printing press next to the guard's starting position:
//
// ....#.....
// ....+---+#
// ....|...|.
// ..#.|...|.
// ....|..#|.
// ....|...|.
// .#.O^---+.
// ........#.
// #.........
// ......#...
//
// Option two, put a stack of failed suit prototypes in the bottom right quadrant of the mapped area:
//
// ....#.....
// ....+---+#
// ....|...|.
// ..#.|...|.
// ..+-+-+#|.
// ..|.|.|.|.
// .#+-^-+-+.
// ......O.#.
// #.........
// ......#...
//
// Option three, put a crate of chimney-squeeze prototype fabric next to the standing desk in the bottom right quadrant:
//
// ....#.....
// ....+---+#
// ....|...|.
// ..#.|...|.
// ..+-+-+#|.
// ..|.|.|.|.
// .#+-^-+-+.
// .+----+O#.
// #+----+...
// ......#...
//
// Option four, put an alchemical retroencabulator near the bottom left corner:
//
// ....#.....
// ....+---+#
// ....|...|.
// ..#.|...|.
// ..+-+-+#|.
// ..|.|.|.|.
// .#+-^-+-+.
// ..|...|.#.
// #O+---+...
// ......#...
//
// Option five, put the alchemical retroencabulator a bit to the right instead:
//
// ....#.....
// ....+---+#
// ....|...|.
// ..#.|...|.
// ..+-+-+#|.
// ..|.|.|.|.
// .#+-^-+-+.
// ....|.|.#.
// #..O+-+...
// ......#...
//
// Option six, put a tank of sovereign glue right next to the tank of universal solvent:
//
// ....#.....
// ....+---+#
// ....|...|.
// ..#.|...|.
// ..+-+-+#|.
// ..|.|.|.|.
// .#+-^-+-+.
// .+----++#.
// #+----++..
// ......#O..
//
// It doesn't really matter what you choose to use as an obstacle so long as you and The Historians can put it into position without the guard noticing. The important thing is having enough options that you can find one that minimizes time paradoxes, and in this example, there are 6 different positions you could choose.
//
// You need to get the guard stuck in a loop by adding a single new obstruction. How many different positions could you choose for this obstruction?

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

type TravelledDirections = {
    North: bool
    East: bool
    South: bool
    West: bool
}

type Entity =
    | EmptySpace
    | Obstacle
    | Travelled of TravelledDirections
    | Guard of Direction option * TravelledDirections

let rawMap = File.ReadAllLines "../day-06-part-1/input.txt"

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
let emptyTravelledDirection = { North = false; East = false; South = false; West = false }
let startingTravelledDirection = { emptyTravelledDirection with North = true }
let mutable guardPosition = (Guard ((Some North), startingTravelledDirection), { X = 0; Y = 0 })

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
                                guardPosition <- (Guard ((Some North), startingTravelledDirection), { X = xIndex; Y = yIndex })
                                Guard ((Some North), startingTravelledDirection)
                            | _ -> raise <| ApplicationException($"Unexpected char: {ch}")

                        map[xIndex, yIndex] <- entity))

let startingGuardPosition = guardPosition
let startingMap = Array2D.copy map

let newTravelled (old: TravelledDirections) (newDirection: Direction) =
    match newDirection with
    | North when not old.North -> { old with North = true }
    | East when not old.East   -> { old with East = true }
    | South when not old.South -> { old with South = true }
    | West when not old.West   -> { old with West = true }
    | _                        -> old

let rec checkPathForTravelled (dir: Direction) (startingPoint: Point) =
    match dir with
    | North ->
        match map[startingPoint.X, startingPoint.Y] with
        | Obstacle -> false
        | Travelled { North = true } -> true
        | _ -> if startingPoint.Y - 1 >= 0 then checkPathForTravelled dir { startingPoint with Y = startingPoint.Y - 1 } else false
    | East ->
        match map[startingPoint.X, startingPoint.Y] with
        | Obstacle -> false
        | Travelled { East = true } -> true
        | _ -> if startingPoint.X + 1 < sizeX then checkPathForTravelled dir { startingPoint with X = startingPoint.X + 1 } else false
    | South ->
        match map[startingPoint.X, startingPoint.Y] with
        | Obstacle -> false
        | Travelled { South = true } -> true
        | _ -> if startingPoint.Y + 1 < sizeY then checkPathForTravelled dir { startingPoint with Y = startingPoint.Y + 1 } else false
    | West ->
        match map[startingPoint.X, startingPoint.Y] with
        | Obstacle -> false
        | Travelled { West = true } -> true
        | _ -> if startingPoint.X - 1 >= 0 then checkPathForTravelled dir { startingPoint with X = startingPoint.X - 1 } else false

let checkForLoop (map: Entity array2d) (direction: Direction) (point: Point) =
    match direction with
    | North ->
        let pointToEvaluate = { point with Y = point.Y - 1 }

        if pointToEvaluate.Y >= 0 then
            match map[pointToEvaluate.X, pointToEvaluate.Y] with
            | Travelled { North = true } -> true
            | _ -> false
        else
            false
    | East ->
        let pointToEvaluate = { point with X = point.X + 1 }

        if pointToEvaluate.X < sizeX then
            match map[pointToEvaluate.X, pointToEvaluate.Y] with
            | Travelled { East = true } -> true
            | _ -> false
        else
            false
    | South ->
        let pointToEvaluate = { point with Y = point.Y + 1 }

        if pointToEvaluate.Y < sizeY then
            match map[pointToEvaluate.X, pointToEvaluate.Y] with
            | Travelled { South = true } -> true
            | _ -> false
        else
            false
    | West ->
        let pointToEvaluate = { point with X = point.X - 1 }

        if pointToEvaluate.X >= 0 then
            match map[pointToEvaluate.X, pointToEvaluate.Y] with
            | Travelled { West = true } -> true
            | _ -> false
        else
            false

let tick (map: Entity array2d) =
    let (guard, guardPoint) = guardPosition

    match guard with
    | Guard ((Some North), travelledDirection) ->
        let newY = guardPoint.Y - 1

        if newY = -1 then
            map[guardPoint.X, guardPoint.Y] <- Travelled travelledDirection
            guardPosition <- (Guard (None, travelledDirection), guardPoint)
            false
        else
            let newEntity = map[guardPoint.X, newY]

            match newEntity with
            | Obstacle ->
                guardPosition <- (Guard ((Some East), newTravelled travelledDirection East), guardPoint)
                false
            | EmptySpace ->
                let newGuard = Guard ((Some North), newTravelled emptyTravelledDirection North)

                map[guardPoint.X, guardPoint.Y] <- Travelled travelledDirection
                map[guardPoint.X, newY] <- newGuard
                guardPosition <- (newGuard, { X = guardPoint.X; Y = newY })
                false
            | Travelled dir ->
                let newGuard = Guard ((Some North), newTravelled dir North)

                if checkForLoop map North guardPoint then
                    true
                else
                    map[guardPoint.X, guardPoint.Y] <- Travelled travelledDirection
                    map[guardPoint.X, newY] <- newGuard
                    guardPosition <- (newGuard, { X = guardPoint.X; Y = newY })
                    false
            | _ -> false
    | Guard ((Some East), travelledDirection) ->
        let newX = guardPoint.X + 1

        if newX = sizeX then
            map[guardPoint.X, guardPoint.Y] <- Travelled travelledDirection
            guardPosition <- (Guard (None, travelledDirection), guardPoint)
            false
        else
            let newEntity = map[newX, guardPoint.Y]

            match newEntity with
            | Obstacle ->
                guardPosition <- (Guard ((Some South), newTravelled travelledDirection South), guardPoint)
                false
            | EmptySpace ->
                let newGuard = Guard ((Some East), newTravelled emptyTravelledDirection East)

                map[guardPoint.X, guardPoint.Y] <- Travelled travelledDirection
                map[newX, guardPoint.Y] <- newGuard
                guardPosition <- (newGuard, { X = newX; Y = guardPoint.Y })
                false
            | Travelled dir ->
                let newGuard = Guard ((Some East), newTravelled dir East)

                if checkForLoop map East guardPoint then
                    true
                else
                    map[guardPoint.X, guardPoint.Y] <- Travelled travelledDirection
                    map[newX, guardPoint.Y] <- newGuard
                    guardPosition <- (newGuard, { X = newX; Y = guardPoint.Y })
                    false
            | _ -> false
    | Guard ((Some South), travelledDirection) ->
        let newY = guardPoint.Y + 1

        if newY = sizeY then
            map[guardPoint.X, guardPoint.Y] <- Travelled travelledDirection
            guardPosition <- (Guard (None, travelledDirection), guardPoint)
            false
        else
            let newEntity = map[guardPoint.X, newY]

            match newEntity with
            | Obstacle ->
                guardPosition <- (Guard ((Some West), newTravelled travelledDirection West), guardPoint)
                false
            | EmptySpace ->
                let newGuard = Guard ((Some South), newTravelled emptyTravelledDirection South)

                map[guardPoint.X, guardPoint.Y] <- Travelled travelledDirection
                map[guardPoint.X, newY] <- newGuard
                guardPosition <- (newGuard, { X = guardPoint.X; Y = newY })
                false
            | Travelled dir ->
                let newGuard = Guard ((Some South), newTravelled dir South)

                if checkForLoop map South guardPoint then
                    true
                else
                    map[guardPoint.X, guardPoint.Y] <- Travelled travelledDirection
                    map[guardPoint.X, newY] <- newGuard
                    guardPosition <- (newGuard, { X = guardPoint.X; Y = newY })
                    false
            | _ -> false
    | Guard ((Some West), travelledDirection) ->
        let newX = guardPoint.X - 1

        if newX = -1 then
            map[guardPoint.X, guardPoint.Y] <- Travelled travelledDirection
            guardPosition <- (Guard (None, travelledDirection), guardPoint)
            false
        else
            let newEntity = map[newX, guardPoint.Y]

            match newEntity with
            | Obstacle ->
                guardPosition <- (Guard ((Some North), newTravelled travelledDirection North), guardPoint)
                false
            | EmptySpace ->
                let newGuard = Guard ((Some West), newTravelled emptyTravelledDirection West)

                map[guardPoint.X, guardPoint.Y] <- Travelled travelledDirection
                map[newX, guardPoint.Y] <- newGuard
                guardPosition <- (newGuard, { X = newX; Y = guardPoint.Y })
                false
            | Travelled dir ->
                let newGuard = Guard ((Some West), newTravelled dir West)

                if checkForLoop map West guardPoint then
                    true
                else
                    map[guardPoint.X, guardPoint.Y] <- Travelled travelledDirection
                    map[newX, guardPoint.Y] <- newGuard
                    guardPosition <- (newGuard, { X = newX; Y = guardPoint.Y })
                    false
            | _ -> false
    | _ -> false

let infiniteSeq () =
    let rec innerSeq n =
        seq {
            yield n
            yield! innerSeq (n + 1L)
        }

    innerSeq 1L

let walkMap (thisMap: Entity array2d)  =
    let mutable wasLoop = false

    guardPosition <- startingGuardPosition

    infiniteSeq()
        |> Seq.takeWhile
            (fun i ->
                let isLoop = tick thisMap

                match (isLoop, fst guardPosition) with
                | (true, _) ->
                    wasLoop <- true
                    false
                | (_, Guard (None, _)) -> false
                | _ -> true)
        |> Seq.iter (fun _ -> ())

    wasLoop

let initialMap = Array2D.copy startingMap

walkMap initialMap |> ignore

let newObstacleCandidates =
    initialMap
        |> Array2D.mapi (fun x y entity -> (x, y, entity))
        |> Seq.cast<(int * int * Entity)>
        |> Seq.filter
            (fun (x, y, entity) ->
                match entity with
                | Travelled _ -> (snd startingGuardPosition) <> { X = x; Y = y }
                | _ -> false)
        |> Seq.map (fun (x, y, _) -> { X = x; Y = y })

let newObstacles =
    newObstacleCandidates
        |> Seq.filter
            (fun point ->
                let newMap = Array2D.copy startingMap

                newMap[point.X, point.Y] <- Obstacle
                walkMap newMap)

printfn "%A" (newObstacles |> Seq.length)
