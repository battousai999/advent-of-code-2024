// During the bathroom break, someone notices that these robots seem awfully similar to ones built and used at the North
// Pole. If they're the same type of robots, they should have a hard-coded Easter egg: very rarely, most of the robots
// should arrange themselves into a picture of a Christmas tree.
//
// What is the fewest number of seconds that must elapse for the robots to display the Easter egg?

open System
open System.IO
open Common
open System.Text.RegularExpressions

type Vector = {
    X: int
    Y: int
}

type Robot = {
    InitialPosition: Vector
    Velocity: Vector
}

let maxX = 101 //11
let maxY = 103 //7

let robotRegex = Regex(@"^p=(\d+),(\d+)\sv=([\d-]+),([\d-]+)$")

let rawRobots = File.ReadAllLines("../day-14-part-1/input.txt")

// let rawRobotsStr = @"p=0,4 v=3,-3
// p=6,3 v=-1,-3
// p=10,3 v=-1,2
// p=2,0 v=2,-1
// p=0,0 v=1,3
// p=3,0 v=-2,-2
// p=7,6 v=-1,-3
// p=3,0 v=-1,-2
// p=9,3 v=2,3
// p=7,3 v=-1,2
// p=2,4 v=2,-3
// p=9,5 v=-3,-3"

// let rawRobots = rawRobotsStr.Split(Environment.NewLine)

let robots =
    rawRobots
        |> Array.map
            (fun line ->
                match line with
                | Regexer robotRegex [posX; posY; velX; velY] ->
                    { InitialPosition = { X = int posX; Y = int posY }; Velocity = { X = int velX; Y = int velY }}
                | _ -> raise <| ApplicationException($"Invalid input line: {line}"))
        |> Seq.ofArray

let simulate iterations robot =
    let newX = (robot.InitialPosition.X + (iterations * robot.Velocity.X)) % maxX
    let newY = (robot.InitialPosition.Y + (iterations * robot.Velocity.Y)) % maxY

    {
        X = if newX < 0 then newX + maxX else newX
        Y = if newY < 0 then newY + maxY else newY
    }

// Looks for runs of increasing odd-numbered robots (up to a certain run size)
let isChristmasTree (grid: Vector seq) =
    let longestRun (row: Vector seq) =
        let sorted = row |> Seq.sortBy _.X

        let (_, _, run) =
            sorted
                |> Seq.fold
                    (fun (lastX, currentCount, largestCount) vector ->
                        if vector.X = lastX + 1 then
                            let newCount = currentCount + 1
                            (vector.X, newCount, if newCount > largestCount then newCount else largestCount)
                        else
                            (vector.X, 1, largestCount))
                    (-1, 0, 0)

        run

    let upperRunSize = 11
    let rows = grid |> Seq.groupBy _.Y |> Seq.sortBy fst |> Seq.map snd
    let longestRuns = rows |> Seq.map longestRun |> List.ofSeq

    [2..upperRunSize]
        |> List.filter isOdd
        |> List.forall (fun size -> longestRuns |> List.exists (fun x -> x = size))

let steps =
    infiniteSeq()
        |> Seq.find
            (fun i ->
                let grid = robots |> Seq.map (simulate (int i))

                (isChristmasTree grid) || i > 10000000)

printfn "%d" steps
