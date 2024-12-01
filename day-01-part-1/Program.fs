// Throughout the Chief's office, the historically significant locations are listed not by name but by a unique number called the
// location ID. To make sure they don't miss anything, The Historians split into two groups, each searching the office and trying
// to create their own complete list of location IDs.
//
// There's just one problem: by holding the two lists up side by side (your puzzle input), it quickly becomes clear that the lists
// aren't very similar. Maybe you can help The Historians reconcile their lists?
//
// For example:
// 3   4
// 4   3
// 2   5
// 1   3
// 3   9
// 3   3
//
// Maybe the lists are only off by a small amount! To find out, pair up the numbers and measure how far apart they are. Pair up the
// smallest number in the left list with the smallest number in the right list, then the second-smallest left number with the second-smallest
// right number, and so on.
//
// Within each pair, figure out how far apart the two numbers are; you'll need to add up all of those distances. For example, if you pair up
// a 3 from the left list with a 7 from the right list, the distance apart is 4; if you pair up a 9 with a 3, the distance apart is 6.
//
// In the example list above, the pairs and distances would be as follows:
//
// - The smallest number in the left list is 1, and the smallest number in the right list is 3. The distance between them is 2.
// - The second-smallest number in the left list is 2, and the second-smallest number in the right list is another 3. The distance between them is 1.
// - The third-smallest number in both lists is 3, so the distance between them is 0.
// - The next numbers to pair up are 3 and 4, a distance of 1.
// - The fifth-smallest numbers in each list are 3 and 5, a distance of 2.
// - Finally, the largest number in the left list is 4, while the largest number in the right list is 9; these are a distance 5 apart.
//
// To find the total distance between the left list and the right list, add up the distances between all of the pairs you found. In the example
// above, this is 2 + 1 + 0 + 1 + 2 + 5, a total distance of 11!
//
// Your actual left and right lists contain many location IDs. What is the total distance between your lists?

open System
open System.IO
open System.Text.RegularExpressions


let (|Regexer|_|) (regex : Regex) input =
    let m = regex.Match(input)

    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let lineRegex = Regex(@"^(\d+)\s+(\d+)$")

let lines = File.ReadAllLines "./input.txt"

// let lines =
//     [|
//         "3   4";
//         "4   3";
//         "2   5";
//         "1   3";
//         "3   9";
//         "3   3";
//     |]

let (list1, list2) =
    lines
        |> Array.map
            (fun line ->
                match line with
                | Regexer lineRegex [x1; x2] -> (x1 |> int, x2 |> int)
                | _ -> raise <| ApplicationException($"invalid line: {line}"))
        |> Array.unzip

let sortedList1 = list1 |> Seq.ofArray |> Seq.sort
let sortedList2 = list2 |> Seq.ofArray |> Seq.sort

let distances = Seq.zip sortedList1 sortedList2 |> Seq.map (fun (a, b) -> (a - b) |> abs)

let totalDistance = distances |> Seq.sum

printfn "%d" totalDistance
