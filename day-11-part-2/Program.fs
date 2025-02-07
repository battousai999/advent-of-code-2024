// The Historians sure are taking a long time. To be fair, the infinite corridors are very large.
//
// How many stones would you have after blinking a total of 75 times?

open System
open System.IO
open System.Collections.Concurrent
open Common

type Tree =
    | Branch of Tree * Tree
    | Leaf of int64

type TreeList = Tree list

let rawInitialStones = File.ReadAllText "../day-11-part-1/input.txt"

// let rawInitialStones = "125 17"

let stonesList = rawInitialStones.Split(' ') |> Array.map int64 |> List.ofArray

// Note: needed to translate to the Y-combinator-associated form of my recursive function in order to memoize it more easily
let countStones = memoizeRecursiveFunction <| (fun recursiveFunc ((iterations, stoneValue): (int * int64)) ->
    if iterations = 0 then
        1L
    elif stoneValue = 0 then
        recursiveFunc ((iterations - 1), 1L)
    else
        let valueStr = stoneValue.ToString()

        if valueStr.Length % 2 = 0 then
            let leftValue = valueStr[0..(valueStr.Length / 2)-1] |> int64
            let rightValue = valueStr[valueStr.Length / 2..] |> int64

            (recursiveFunc ((iterations - 1), leftValue)) + (recursiveFunc ((iterations - 1), rightValue))
        else
            recursiveFunc ((iterations - 1), (stoneValue * 2024L)))

let answer = stonesList |> List.sumBy (fun stone -> countStones (75, stone))

printfn "%d" answer
