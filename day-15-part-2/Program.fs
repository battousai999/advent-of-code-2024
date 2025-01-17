// The lanternfish use your information to find a safe moment to swim in and turn off the malfunctioning robot! Just
// as they start preparing a festival in your honor, reports start coming in that a second warehouse's robot is also
// malfunctioning.
//
// This warehouse's layout is surprisingly similar to the one you just helped. There is one key difference: everything
// except the robot is twice as wide! The robot's list of movements doesn't change.
//
// To get the wider warehouse's map, start with your original map and, for each tile, make the following changes:
//
// If the tile is #, the new map contains ## instead.
// If the tile is O, the new map contains [] instead.
// If the tile is ., the new map contains .. instead.
// If the tile is @, the new map contains @. instead.
// This will produce a new warehouse map which is twice as wide and with wide boxes that are represented by []. (The robot
// does not change size.)
//
// The larger example from before would now look like this:
//
// ####################
// ##....[]....[]..[]##
// ##............[]..##
// ##..[][]....[]..[]##
// ##....[]@.....[]..##
// ##[]##....[]......##
// ##[]....[]....[]..##
// ##..[][]..[]..[][]##
// ##........[]......##
// ####################
//
// Because boxes are now twice as wide but the robot is still the same size and speed, boxes can be aligned such that they
// directly push two other boxes at once. For example, consider this situation:
//
// #######
// #...#.#
// #.....#
// #..OO@#
// #..O..#
// #.....#
// #######
//
// <vv<<^^<<^^
//
// After appropriately resizing this map, the robot would push around these boxes as follows:
//
// Initial state:
//
// ##############
// ##......##..##
// ##..........##
// ##....[][]@.##
// ##....[]....##
// ##..........##
// ##############
//
// Move <:
//
// ##############
// ##......##..##
// ##..........##
// ##...[][]@..##
// ##....[]....##
// ##..........##
// ##############
//
// Move v:
//
// ##############
// ##......##..##
// ##..........##
// ##...[][]...##
// ##....[].@..##
// ##..........##
// ##############
//
// Move v:
//
// ##############
// ##......##..##
// ##..........##
// ##...[][]...##
// ##....[]....##
// ##.......@..##
// ##############
//
// Move <:
//
// ##############
// ##......##..##
// ##..........##
// ##...[][]...##
// ##....[]....##
// ##......@...##
// ##############
//
// Move <:
//
// ##############
// ##......##..##
// ##..........##
// ##...[][]...##
// ##....[]....##
// ##.....@....##
// ##############
//
// Move ^:
//
// ##############
// ##......##..##
// ##...[][]...##
// ##....[]....##
// ##.....@....##
// ##..........##
// ##############
//
// Move ^:
//
// ##############
// ##......##..##
// ##...[][]...##
// ##....[]....##
// ##.....@....##
// ##..........##
// ##############
//
// Move <:
//
// ##############
// ##......##..##
// ##...[][]...##
// ##....[]....##
// ##....@.....##
// ##..........##
// ##############
//
// Move <:
//
// ##############
// ##......##..##
// ##...[][]...##
// ##....[]....##
// ##...@......##
// ##..........##
// ##############
//
// Move ^:
//
// ##############
// ##......##..##
// ##...[][]...##
// ##...@[]....##
// ##..........##
// ##..........##
// ##############
//
// Move ^:
//
// ##############
// ##...[].##..##
// ##...@.[]...##
// ##....[]....##
// ##..........##
// ##..........##
// ##############
//
// This warehouse also uses GPS to locate the boxes. For these larger boxes, distances are measured from the edge of the
// map to the closest edge of the box in question. So, the box shown below has a distance of 1 from the top edge of the map
// and 5 from the left edge of the map, resulting in a GPS coordinate of 100 * 1 + 5 = 105.
//
// ##########
// ##...[]...
// ##........
//
// In the scaled-up version of the larger example from above, after the robot has finished all of its moves, the warehouse
// would look like this:
//
// ####################
// ##[].......[].[][]##
// ##[]...........[].##
// ##[]........[][][]##
// ##[]......[]....[]##
// ##..##......[]....##
// ##..[]............##
// ##..@......[].[][]##
// ##......[][]..[]..##
// ####################
//
// The sum of these boxes' GPS coordinates is 9021.
//
// Predict the motion of the robot and boxes in this new, scaled-up warehouse. What is the sum of all boxes' final GPS coordinates?

open System
open System.IO
open Common

type Point = {
    X: int
    Y: int
}

type MapElement =
    | Empty
    | Wall
    | Robot
    | BoxLeft
    | BoxRight

type Direction =
    | North
    | East
    | South
    | West

let rawInput = File.ReadAllText("../day-15-part-1/input.txt")

// let rawInput = @"##########
// #..O..O.O#
// #......O.#
// #.OO..O.O#
// #..O@..O.#
// #O#..O...#
// #O..O..O.#
// #.OO.O.OO#
// #....O...#
// ##########

// <vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
// vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
// ><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
// <<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
// ^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
// ^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
// >^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
// <><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
// ^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
// v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"

let splitInput = rawInput.Split(Environment.NewLine + Environment.NewLine)

let rawMap =
    splitInput[0]
        .Replace("#", "##")
        .Replace("O", "[]")
        .Replace(".", "..")
        .Replace("@", "@.")
        .Split(Environment.NewLine)

let rawDirections = splitInput[1].ReplaceLineEndings("")

let map =
    rawMap
    |> buildMap
        Empty
        (fun ch ->
            match ch with
            | '#' -> Wall
            | '.' -> Empty
            | '@' -> Robot
            | '[' -> BoxLeft
            | ']' -> BoxRight
            | _ -> raise <| ApplicationException($"Unexpected character in map: {ch}"))

let mutable robotPosition =
    map
    |> findPositionInMap (fun x -> x = Robot)
    |> Option.map (fun (x, y) -> { X = x; Y = y })
    |> Option.get

let renderMap () =
    map
    |> Common.renderMap
        (fun x ->
            match x with
            | Wall     -> '#'
            | Empty    -> '.'
            | Robot    -> '@'
            | BoxLeft  -> '['
            | BoxRight -> ']')

let directions =
    rawDirections
    |> toChars
    |> Seq.map
        (fun ch ->
            match ch with
            | '^' -> North
            | '>' -> East
            | 'v' -> South
            | '<' -> West
            | _ -> raise <| ApplicationException($"Unexpeted character in directions: {ch}"))

let getPositionInDirection direction position =
    match direction with
    | North -> { X = position.X;     Y = position.Y - 1 }
    | East  -> { X = position.X + 1; Y = position.Y     }
    | South -> { X = position.X;     Y = position.Y + 1 }
    | West  -> { X = position.X - 1; Y = position.Y     }

let moveRobot (map: MapElement array2d) direction =
    let rec moveTo position element isMoving =
        let destElement = map[position.X, position.Y]

        match destElement with
        | Wall ->
            false
        | Empty ->
            if isMoving then
                map[position.X, position.Y] <- element

            true
        | Robot ->
            raise <| ApplicationException($"There can be only one robot")
        | BoxLeft
        | BoxRight ->
            match direction with
            | East
            | West ->
                let newPosition = getPositionInDirection direction position

                if moveTo newPosition destElement isMoving then
                    if isMoving then
                        map[position.X, position.Y] <- element

                    true
                else
                    false
            | North
            | South ->
                let otherBoxElementOffset = if destElement = BoxLeft then 1 else -1
                let newPosition1 = getPositionInDirection direction position
                let newPosition2 = getPositionInDirection direction { position with X = position.X + otherBoxElementOffset }

                let move1 = moveTo newPosition1 destElement isMoving
                let move2 = moveTo newPosition2 (if destElement = BoxLeft then BoxRight else BoxLeft) isMoving

                if move1 && move2 then
                    if isMoving then
                        map[position.X, position.Y] <- element
                        map[position.X + otherBoxElementOffset, position.Y] <- Empty

                    true
                else
                    false

    let startingPosition = robotPosition
    let newPosition = getPositionInDirection direction robotPosition

    if moveTo newPosition Robot false then
        moveTo newPosition Robot true |> ignore
        map[startingPosition.X, startingPosition.Y] <- Empty
        robotPosition <- newPosition

directions
|> Seq.iter (moveRobot map)

renderMap()

let getGpsCoordinate x y = (y * 100) + x

let totalCoordinates =
    map
    |> Array2D.mapi (fun x y element -> if element = BoxLeft then getGpsCoordinate x y else 0)
    |> toSeq
    |> Seq.sum

printfn "%d" totalCoordinates


