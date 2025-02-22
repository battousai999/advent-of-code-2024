// As you teleport onto Santa's Reindeer-class starship, The Historians begin to panic: someone from their search party is missing. A quick life-form scan by the ship's computer reveals that when the missing Historian teleported, he arrived in another part of the ship.
//
// The door to that area is locked, but the computer can't open it; it can only be opened by physically typing the door codes (your puzzle input) on the numeric keypad on the door.
//
// The numeric keypad has four rows of buttons: 789, 456, 123, and finally an empty gap followed by 0A. Visually, they are arranged like this:
//
// +---+---+---+
// | 7 | 8 | 9 |
// +---+---+---+
// | 4 | 5 | 6 |
// +---+---+---+
// | 1 | 2 | 3 |
// +---+---+---+
//     | 0 | A |
//     +---+---+
//
// Unfortunately, the area outside the door is currently depressurized and nobody can go near the door. A robot needs to be sent instead.
//
// The robot has no problem navigating the ship and finding the numeric keypad, but it's not designed for button pushing: it can't be told to push a specific button directly. Instead, it has a robotic arm that can be controlled remotely via a directional keypad.
//
// The directional keypad has two rows of buttons: a gap / ^ (up) / A (activate) on the first row and < (left) / v (down) / > (right) on the second row. Visually, they are arranged like this:
//
//     +---+---+
//     | ^ | A |
// +---+---+---+
// | < | v | > |
// +---+---+---+
//
// When the robot arrives at the numeric keypad, its robotic arm is pointed at the A button in the bottom right corner. After that, this directional keypad remote control must be used to maneuver the robotic arm: the up / down / left / right buttons cause it to move its arm one button in that direction, and the A button causes the robot to briefly move forward, pressing the button being aimed at by the robotic arm.
//
// For example, to make the robot type 029A on the numeric keypad, one sequence of inputs on the directional keypad you could use is:
//
// - < to move the arm from A (its initial position) to 0.
// - A to push the 0 button.
// - ^A to move the arm to the 2 button and push it.
// - >^^A to move the arm to the 9 button and push it.
// - vvvA to move the arm to the A button and push it.
//
// In total, there are three shortest possible sequences of button presses on this directional keypad that would cause the robot to type 029A: <A^A>^^AvvvA, <A^A^>^AvvvA, and <A^A^^>AvvvA.
//
// Unfortunately, the area containing this directional keypad remote control is currently experiencing high levels of radiation and nobody can go near it. A robot needs to be sent instead.
//
// When the robot arrives at the directional keypad, its robot arm is pointed at the A button in the upper right corner. After that, a second, different directional keypad remote control is used to control this robot (in the same way as the first robot, except that this one is typing on a directional keypad instead of a numeric keypad).
//
// There are multiple shortest possible sequences of directional keypad button presses that would cause this robot to tell the first robot to type 029A on the door. One such sequence is v<<A>>^A<A>AvA<^AA>A<vAAA>^A.
//
// Unfortunately, the area containing this second directional keypad remote control is currently -40 degrees! Another robot will need to be sent to type on that directional keypad, too.
//
// There are many shortest possible sequences of directional keypad button presses that would cause this robot to tell the second robot to tell the first robot to eventually type 029A on the door. One such sequence is <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A.
//
// Unfortunately, the area containing this third directional keypad remote control is currently full of Historians, so no robots can find a clear path there. Instead, you will have to type this sequence yourself.
//
// Were you to choose this sequence of button presses, here are all of the buttons that would be pressed on your directional keypad, the two robots' directional keypads, and the numeric keypad:
//
// <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
// v<<A>>^A<A>AvA<^AA>A<vAAA>^A
// <A^A>^^AvvvA
// 029A
//
// In summary, there are the following keypads:
//
// - One directional keypad that you are using.
// - Two directional keypads that robots are using.
// - One numeric keypad (on a door) that a robot is using.
//
// It is important to remember that these robots are not designed for button pushing. In particular, if a robot arm is ever aimed at a gap where no button is present on the keypad, even for an instant, the robot will panic unrecoverably. So, don't do that. All robots will initially aim at the keypad's A key, wherever it is.
//
// To unlock the door, five codes will need to be typed on its numeric keypad. For example:
//
// 029A
// 980A
// 179A
// 456A
// 379A
//
// For each of these, here is a shortest sequence of button presses you could type to cause the desired code to be typed on the numeric keypad:
//
// 029A: <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
// 980A: <v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A
// 179A: <v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
// 456A: <v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A
// 379A: <v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
//
// The Historians are getting nervous; the ship computer doesn't remember whether the missing Historian is trapped in the area containing a giant electromagnet or molten lava. You'll need to make sure that for each of the five codes, you find the shortest sequence of button presses necessary.
//
// The complexity of a single code (like 029A) is equal to the result of multiplying these two values:
//
// - The length of the shortest sequence of button presses you need to type on your directional keypad in order to cause the code to be typed on the numeric keypad; for 029A, this would be 68.
// - The numeric part of the code (ignoring leading zeroes); for 029A, this would be 29.
//
// In the above example, complexity of the five codes can be found by calculating 68 * 29, 60 * 980, 68 * 179, 64 * 456, and 64 * 379. Adding these together produces 126384.
//
// Find the fewest number of button presses you'll need to perform in order to cause the robot in front of the door to type each code. What is the sum of the complexities of the five codes on your list?

open System
open System.IO
open Common
open System.Collections.Generic

type NumericKeypad =
| Numeric_0 = 0
| Numeric_1 = 1
| Numeric_2 = 2
| Numeric_3 = 3
| Numeric_4 = 4
| Numeric_5 = 5
| Numeric_6 = 6
| Numeric_7 = 7
| Numeric_8 = 8
| Numeric_9 = 9
| Numeric_A = 10

type DirectionalKeypad =
| Directional_Up    = 0
| Directional_Right = 1
| Directional_Down  = 2
| Directional_Left  = 3
| Directional_A     = 4

type KeypadAction =
| MoveUp
| MoveRight
| MoveDown
| MoveLeft
| PressButton

// let rawCodes = File.ReadAllLines @"./input.txt"

let rawCodesStr = @"";

let rawCodes = rawCodesStr.Split Environment.NewLine

let codes =
    rawCodes
    |> Array.map
        (fun line ->
            line
            |> Seq.map
                (fun ch ->
                    match ch with
                    | '0' -> NumericKeypad.Numeric_0
                    | '1' -> NumericKeypad.Numeric_1
                    | '2' -> NumericKeypad.Numeric_2
                    | '3' -> NumericKeypad.Numeric_3
                    | '4' -> NumericKeypad.Numeric_4
                    | '5' -> NumericKeypad.Numeric_5
                    | '6' -> NumericKeypad.Numeric_6
                    | '7' -> NumericKeypad.Numeric_7
                    | '8' -> NumericKeypad.Numeric_8
                    | '9' -> NumericKeypad.Numeric_9
                    | 'A' -> NumericKeypad.Numeric_A
                    | _ -> raise <| ApplicationException $"Invalid input character: {ch}")
            |> List.ofSeq)
    |> List.ofArray

let numericKeypadGraph =
    let v0 = { Data = NumericKeypad.Numeric_0 }
    let v1 = { Data = NumericKeypad.Numeric_1 }
    let v2 = { Data = NumericKeypad.Numeric_2 }
    let v3 = { Data = NumericKeypad.Numeric_3 }
    let v4 = { Data = NumericKeypad.Numeric_4 }
    let v5 = { Data = NumericKeypad.Numeric_5 }
    let v6 = { Data = NumericKeypad.Numeric_6 }
    let v7 = { Data = NumericKeypad.Numeric_7 }
    let v8 = { Data = NumericKeypad.Numeric_8 }
    let v9 = { Data = NumericKeypad.Numeric_9 }
    let vA = { Data = NumericKeypad.Numeric_A }

    let vertices = [v0; v1; v2; v3; v4; v5; v6; v7; v8; v9; vA]

    let edge source dest = { Source = source; Dest = dest; Weight = 1 }

    let edges = [
        edge vA v0; edge vA v3;
        edge v0 vA; edge v0 v2;
        edge v1 v2; edge v1 v4;
        edge v2 v0; edge v2 v1; edge v2 v5; edge v2 v3;
        edge v3 vA; edge v3 v2; edge v3 v6;
        edge v4 v1; edge v4 v5; edge v4 v7;
        edge v5 v2; edge v5 v4; edge v4 v6; edge v4 v8;
        edge v6 v3; edge v6 v5; edge v6 v9;
        edge v7 v4; edge v7 v8;
        edge v8 v5; edge v8 v7; edge v8 v9;
        edge v9 v6; edge v9 v8;
    ]

    { Vertices = vertices; Edges = edges }

let directionalKeypadGraph =
    let vU = { Data = DirectionalKeypad.Directional_Up }
    let vR = { Data = DirectionalKeypad.Directional_Right }
    let vD = { Data = DirectionalKeypad.Directional_Down }
    let vL = { Data = DirectionalKeypad.Directional_Left }
    let vA = { Data = DirectionalKeypad.Directional_A }

    let vertices = [ vU; vR; vD; vL; vA ]

    let edge source dest = { Source = source; Dest = dest; Weight = 1 }

    let edges = [
        edge vA vU; edge vA vR;
        edge vU vA; edge vU vD;
        edge vR vA; edge vR vD;
        edge vD vU; edge vD vR; edge vD vL;
        edge vL vD;
    ]

    { Vertices = vertices; Edges = edges }


// TODO: May need to refactor this to take a Vertex<DirectedKeypad> as input
let getNumericKeypadActions path =
    let firstKey = path |> List.map _.Data |> List.head

    path
    |> List.skip 1
    |> List.map _.Data
    |> List.fold
        (fun (prevKey, results) key ->
            let action =
                match (prevKey, key) with
                | NumericKeypad.Numeric_A, NumericKeypad.Numeric_0 -> MoveLeft
                | NumericKeypad.Numeric_A, NumericKeypad.Numeric_3 -> MoveUp

                | NumericKeypad.Numeric_0, NumericKeypad.Numeric_A -> MoveRight
                | NumericKeypad.Numeric_0, NumericKeypad.Numeric_2 -> MoveUp

                | NumericKeypad.Numeric_1, NumericKeypad.Numeric_2 -> MoveRight
                | NumericKeypad.Numeric_1, NumericKeypad.Numeric_4 -> MoveUp

                | NumericKeypad.Numeric_2, NumericKeypad.Numeric_0 -> MoveDown
                | NumericKeypad.Numeric_2, NumericKeypad.Numeric_1 -> MoveLeft
                | NumericKeypad.Numeric_2, NumericKeypad.Numeric_3 -> MoveRight
                | NumericKeypad.Numeric_2, NumericKeypad.Numeric_5 -> MoveUp

                | NumericKeypad.Numeric_3, NumericKeypad.Numeric_A -> MoveDown
                | NumericKeypad.Numeric_3, NumericKeypad.Numeric_2 -> MoveLeft
                | NumericKeypad.Numeric_3, NumericKeypad.Numeric_6 -> MoveUp

                | NumericKeypad.Numeric_4, NumericKeypad.Numeric_1 -> MoveDown
                | NumericKeypad.Numeric_4, NumericKeypad.Numeric_5 -> MoveRight
                | NumericKeypad.Numeric_4, NumericKeypad.Numeric_7 -> MoveUp

                | NumericKeypad.Numeric_5, NumericKeypad.Numeric_2 -> MoveDown
                | NumericKeypad.Numeric_5, NumericKeypad.Numeric_4 -> MoveLeft
                | NumericKeypad.Numeric_5, NumericKeypad.Numeric_6 -> MoveRight
                | NumericKeypad.Numeric_5, NumericKeypad.Numeric_8 -> MoveUp

                | NumericKeypad.Numeric_6, NumericKeypad.Numeric_3 -> MoveDown
                | NumericKeypad.Numeric_6, NumericKeypad.Numeric_5 -> MoveLeft
                | NumericKeypad.Numeric_6, NumericKeypad.Numeric_9 -> MoveUp

                | NumericKeypad.Numeric_7, NumericKeypad.Numeric_4 -> MoveDown
                | NumericKeypad.Numeric_7, NumericKeypad.Numeric_8 -> MoveRight

                | NumericKeypad.Numeric_8, NumericKeypad.Numeric_5 -> MoveDown
                | NumericKeypad.Numeric_8, NumericKeypad.Numeric_7 -> MoveLeft
                | NumericKeypad.Numeric_8, NumericKeypad.Numeric_9 -> MoveRight

                | NumericKeypad.Numeric_9, NumericKeypad.Numeric_6 -> MoveDown
                | NumericKeypad.Numeric_9, NumericKeypad.Numeric_8 -> MoveLeft

                | _ -> raise <| ApplicationException $"Unexpected numeric direction: {prevKey} to {key}"

            (key, action :: results))
        (firstKey, [])
    ||> (fun _ results -> (KeypadAction.PressButton :: results) |> List.rev)

type Direction =
| North
| East
| South
| West

type DirectedVertex<'a when 'a: equality> = {
    Vertex: Vertex<'a>
    Direction: Direction
}

type State<'a when 'a: equality> = {
    Cost: int
    DirectedVertex: DirectedVertex<'a>
}


// Modified from Common.dijkstra to keep multiple, same-cost paths in the prevMap (from day 16)
let dijkstra<'a when 'a: equality>
    (graph: Graph<'a>)
    (source: Vertex<'a>)
    (target: Vertex<'a>) =
    let distanceMap = Dictionary<DirectedVertex<'a>, int>()
    let prevMap = Dictionary<DirectedVertex<'a>, ResizeArray<DirectedVertex<'a>>>()
    let queue = PriorityQueue<State<'a>, int>()
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

let getActionsSetsForNumericKeypad (keys: NumericKeypad list) =
    keys
    |> List.fold
        (fun (sourceKey, actions) key ->
            let shortestPath =
                let sourceVertex = { Data = sourceKey }
                let destVertex = { Data = key }
                let pathResults = dijkstra numericKeypadGraph sourceVertex destVertex

                getShortestPaths sourceVertex destVertex pathResults.PrevMap

            let newActions = getNumericKeypadActions shortestPath

            (key, newActions :: actions))
        (NumericKeypad.Numeric_A, [])
    ||> (fun _ results -> results |> List.rev)
    |> List.concat

let temp = getActionsForNumericKeypad [
    NumericKeypad.Numeric_0;
    NumericKeypad.Numeric_2;
    NumericKeypad.Numeric_9;
    NumericKeypad.Numeric_A;
]

printfn "%A" temp


// TODO: getting this custom dijksra to work with both NumericKeypad and DirectionalKeypad may require
//       the use of a discriminated union, where the graphs will be of Graph<DirectedKeypad>
type DirectedKeypad =
| Numeric of NumericKeypad
| Directional of DirectionalKeypad