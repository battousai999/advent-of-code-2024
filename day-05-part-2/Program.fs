// While the Elves get to work printing the correctly-ordered updates, you have a little time to
// fix the rest of them.
//
// For each of the incorrectly-ordered updates, use the page ordering rules to put the page numbers
// in the right order. For the above example, here are the three incorrectly-ordered updates and their
// correct orderings:
//
// - 75,97,47,61,53 becomes 97,75,47,61,53.
// - 61,13,29 becomes 61,29,13.
// - 97,13,75,29,47 becomes 97,75,47,29,13.
//
// After taking only the incorrectly-ordered updates and ordering them correctly, their middle page
// numbers are 47, 29, and 47. Adding these together produces 123.
//
// Find the updates which are not in the correct order. What do you get if you add up the middle page
// numbers after correctly ordering just those updates?

open System
open System.IO
open System.Text.RegularExpressions

let (|Regexer|_|) (regex : Regex) input =
    let m = regex.Match(input)

    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let orderingRegex = Regex(@"^(\d+)\|(\d+)$")

let orderingLines = File.ReadAllLines "../day-05-part-1/ordering-rules.txt"
let updatePagesLines = File.ReadAllLines "../day-05-part-1/update-pages.txt"

// let rawOrderingLines = @"47|53
// 97|13
// 97|61
// 97|47
// 75|29
// 61|13
// 75|53
// 29|13
// 97|29
// 53|29
// 61|53
// 97|53
// 61|29
// 47|13
// 75|47
// 97|75
// 47|61
// 75|61
// 47|29
// 75|13
// 53|13"

// let orderingLines = rawOrderingLines.Split(Environment.NewLine)

// let rawUpdatePagesLines = @"75,47,61,53,29
// 97,61,53,29,13
// 75,29,13
// 75,97,47,61,53
// 61,13,29
// 97,13,75,29,47"

// let updatePagesLines = rawUpdatePagesLines.Split(Environment.NewLine)

let orderingRules =
    orderingLines
        |> Array.map
            (fun line ->
                match line with
                | Regexer orderingRegex [x; y] -> (int x, int y)
                | _ -> raise <| ApplicationException($"Invalid line: {line}"))

let updates =
    updatePagesLines
        |> Array.map (fun x -> x.Split(',') |> List.ofArray)
        |> List.ofArray

let toKey a b = if a < b then (a, b) else (b, a)

let orderingLookup =
    orderingRules
        |> Array.map (fun (a, b) -> (toKey a b, (a, b)))
        |> Map.ofArray

let orderingComparer a b =
    let key = toKey a b
    let rule = orderingLookup |> Map.tryFind key

    rule |> Option.map (fun (x, y) -> a = x)

let rec isUpdateOrdered (update: string list) =
    match update with
    | pageStr :: restStr ->
        let page = int pageStr
        let rest = restStr |> Seq.map int
        let satisfiedForPage = rest |> Seq.forall (fun x -> (orderingComparer page x) |> Option.defaultValue true)

        if satisfiedForPage then isUpdateOrdered restStr else false
    | [] -> true

let comparer (a: string) (b: string) =
    match orderingComparer (int a) (int b) with
    | Some true -> -1
    | Some false -> 1
    | _ -> 0

let unorderedUpdates = updates |> List.filter (not << isUpdateOrdered)
let sortedUnorderedUpdates = unorderedUpdates |> List.map (fun x -> x |> List.sortWith comparer)
let middleValues = sortedUnorderedUpdates |> List.map (fun values -> values.Item(values.Length / 2))

printfn "%A" (middleValues |> List.map int |> List.sum)
