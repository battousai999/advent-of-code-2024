// The Historians sure are taking a long time. To be fair, the infinite corridors are very large.
//
// How many stones would you have after blinking a total of 75 times?

open System
open System.IO
open System.Collections.Generic

type Tree =
    | Branch of Tree * Tree
    | Leaf of int64

type TreeList = Tree list

// let rawInitialStones = File.ReadAllText "./input.txt"

let rawInitialStones = "1"

let initialStones = rawInitialStones.Split(' ') |> Array.map int64 |> List.ofArray

let stonesList = LinkedList<int64>(initialStones)

let iterateList (list: LinkedList<int64>) (f: LinkedList<int64> -> LinkedListNode<int64> option -> LinkedListNode<int64> option) =
    let mutable node = Some list.First

    while node.IsSome do
        node <- f list node

let transformStone (list: LinkedList<int64>) (stone: LinkedListNode<int64> option) =
    match stone with
    | None -> None
    | Some null -> None
    | Some node ->
        if node.Value = 0 then
            node.Value <- 1
            Some node.Next
        else
            let valueStr = node.Value.ToString()

            if valueStr.Length % 2 = 0 then
                let leftValue = valueStr[0..(valueStr.Length / 2)-1] |> int64
                let rightValue = valueStr[valueStr.Length / 2..] |> int64
                let newNode = LinkedListNode<int64>(rightValue)

                node.Value <- leftValue
                list.AddAfter(node, newNode)
                Some newNode.Next
            else
                node.Value <- node.Value * 2024L
                Some node.Next

let iterate n action =
    Seq.init n (fun i -> i)
        |> Seq.iter (fun i -> action(); printfn "%d" (i+1))

// lol... can't brute force this one :)
iterate 75 (fun () -> iterateList stonesList transformStone)

printfn "%d" stonesList.Count
