// Fortunately, the Elves are trying to order so much fence that they qualify for a bulk discount!
//
// Under the bulk discount, instead of using the perimeter to calculate the price, you need to use the number of sides each region has. Each straight section of fence counts as a side, regardless of how long it is.
//
// Consider this example again:
//
// AAAA
// BBCD
// BBCC
// EEEC
// The region containing type A plants has 4 sides, as does each of the regions containing plants of type B, D, and E. However, the more complex region containing the plants of type C has 8 sides!
//
// Using the new method of calculating the per-region price by multiplying the region's area by its number of sides, regions A through E have prices 16, 16, 32, 4, and 12, respectively, for a total price of 80.
//
// The second example above (full of type X and O plants) would have a total price of 436.
//
// Here's a map that includes an E-shaped region full of type E plants:
//
// EEEEE
// EXXXX
// EEEEE
// EXXXX
// EEEEE
// The E-shaped region has an area of 17 and 12 sides for a price of 204. Including the two regions full of type X plants, this map has a total price of 236.
//
// This map has a total price of 368:
//
// AAAAAA
// AAABBA
// AAABBA
// ABBAAA
// ABBAAA
// AAAAAA
// It includes two regions full of type B plants (each with 4 sides) and a single region full of type A plants (with 4 sides on the outside and 8 more sides on the inside, a total of 12 sides). Be especially careful when counting the fence around regions like the one full of type A plants; in particular, each section of fence has an in-side and an out-side, so the fence does not connect across the middle of the region (where the two B regions touch diagonally). (The Elves would have used the Möbius Fencing Company instead, but their contract terms were too one-sided.)
//
// The larger example from before now has the following updated prices:
//
// A region of R plants with price 12 * 10 = 120.
// A region of I plants with price 4 * 4 = 16.
// A region of C plants with price 14 * 22 = 308.
// A region of F plants with price 10 * 12 = 120.
// A region of V plants with price 13 * 10 = 130.
// A region of J plants with price 11 * 12 = 132.
// A region of C plants with price 1 * 4 = 4.
// A region of E plants with price 13 * 8 = 104.
// A region of I plants with price 14 * 16 = 224.
// A region of M plants with price 5 * 6 = 30.
// A region of S plants with price 3 * 6 = 18.
// Adding these together produces its new total price of 1206.
//
// What is the new total price of fencing all regions on your map?

open System
open System.Collections.Generic
open System.IO
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

let allDirections = [North; East; South; West]

type Region = {
    Name: char
    Points: Point list
}

let rawMap = File.ReadAllLines "../day-12-part-1/input.txt"

// let rawMapStr = @"RRRRIICCFF
// RRRRIICCCF
// VVRRRCCFFF
// VVRCCCJFFF
// VVVVCJJCFE
// VVIVCCJJEE
// VVIIICJJEE
// MIIIIIJJEE
// MIIISIJEEE
// MMMISSJEEE"

// let rawMap = rawMapStr.Split(Environment.NewLine)

let map = rawMap |> buildMap '.' identity

let xSize = map |> Array2D.length1
let ySize = map |> Array2D.length2

let visited = ResizeArray<Point>()

let getPointInDirection point direction =
    match direction with
    | North when point.Y > 0         -> Some { X = point.X;     Y = point.Y - 1 }
    | South when point.Y < ySize - 1 -> Some { X = point.X;     Y = point.Y + 1 }
    | West  when point.X > 0         -> Some { X = point.X - 1; Y = point.Y     }
    | East  when point.X < xSize - 1 -> Some { X = point.X + 1; Y = point.Y     }
    | _ -> None

let getNonVisitedAdjacentPoints ch point =
    let rec innerGetPoints point currentPoints =
        if visited.Contains point then
            currentPoints
        else
            visited.Add point

            let adjacentPoints = allDirections |> List.choose (getPointInDirection point)
            let nonVisited = adjacentPoints |> List.filter (fun point -> map[point.X, point.Y] = ch && (not <| visited.Contains point))

            List.foldBack (fun x acc -> innerGetPoints x acc) nonVisited (point :: currentPoints)

    innerGetPoints point []

// Determine regions
let regions =
    map
        |> Array2D.mapi (fun x y ch -> ({ X = x; Y = y }, ch))
        |> toSeq
        |> Seq.collect
            (fun (point, ch) ->
                if visited.Contains point then
                    []
                else
                    let points = getNonVisitedAdjacentPoints ch point

                    [{ Name = ch; Points = points }])

let regionSides region =
    if region.Points.Length = 1 then
        4
    else
        let rows = region.Points |> List.groupBy _.Y
        let columns = region.Points |> List.groupBy _.X
        let isNotInRegion point = region.Points |> List.contains point |> not
        let horizontalSides =
            rows
                |> List.sumBy
                    (fun (_, points) ->
                        let topPoints = points |> List.filter (fun p -> getPointInDirection p North |> Option.map isNotInRegion |> Option.defaultValue true)
                        let bottomPoints = points |> List.filter (fun p -> getPointInDirection p South |> Option.map isNotInRegion |> Option.defaultValue true)
                        let countSides points =
                            if List.isEmpty points then
                                0
                            else
                                let orderedPoints = points |> List.sortBy _.X
                                let pairs = orderedPoints |> List.map _.X |> List.pairwise
                                let adjacentPairs = pairs |> List.filter (fun (a, b) -> abs (a - b) = 1)

                                (List.length pairs) - (List.length adjacentPairs) + 1

                        (countSides topPoints) + (countSides bottomPoints))
        let verticalSides =
            columns
                |> List.sumBy
                    (fun (_, points) ->
                        let leftPoints = points |> List.filter (fun p -> getPointInDirection p West |> Option.map isNotInRegion |> Option.defaultValue true)
                        let rightPoints = points |> List.filter (fun p -> getPointInDirection p East |> Option.map isNotInRegion |> Option.defaultValue true)

                        let countSides points =
                            if List.isEmpty points then
                                0
                            else
                                let orderedPoints = points |> List.sortBy _.Y
                                let pairs = orderedPoints |> List.map _.Y |> List.pairwise
                                let adjacentPairs = pairs |> List.filter (fun (a, b) -> abs (a - b) = 1)

                                (List.length pairs) - (List.length adjacentPairs) + 1

                        (countSides leftPoints) + (countSides rightPoints))

        horizontalSides + verticalSides

let answer = regions |> Seq.sumBy (fun region -> (region.Points |> List.length) * (regionSides region))

printfn "%d" answer
