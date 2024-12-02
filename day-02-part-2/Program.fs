// The engineers are surprised by the low number of safe reports until they realize they forgot to tell
// you about the Problem Dampener.
//
// The Problem Dampener is a reactor-mounted module that lets the reactor safety systems tolerate a single
// bad level in what would otherwise be a safe report. It's like the bad level never happened!
//
// Now, the same rules apply as before, except if removing a single level from an unsafe report would make
// it safe, the report instead counts as safe.
//
// More of the above example's reports are now safe:
//
// - 7 6 4 2 1: Safe without removing any level.
// - 1 2 7 8 9: Unsafe regardless of which level is removed.
// - 9 7 6 2 1: Unsafe regardless of which level is removed.
// - 1 3 2 4 5: Safe by removing the second level, 3.
// - 8 6 4 4 1: Safe by removing the third level, 4.
// - 1 3 6 7 9: Safe without removing any level.
//
// Thanks to the Problem Dampener, 4 reports are actually safe!
//
// Update your analysis by handling situations where the Problem Dampener can remove a single level from unsafe
// reports. How many reports are now safe?

open System.IO

let lines = File.ReadAllLines "../day-02-part-1/input.txt"

// let lines =
//     [|
//         "7 6 4 2 1";
//         "1 2 7 8 9";
//         "9 7 6 2 1";
//         "1 3 2 4 5";
//         "8 6 4 4 1";
//         "1 3 6 7 9";
//     |]

let reports =
    lines
        |> Seq.ofArray
        |> Seq.map (fun line -> line.Split(' ') |> Seq.ofArray |> Seq.map (fun x -> int(x)))

let isMonotonic list =
    let pairs = Seq.pairwise list
    let decreasing = pairs |> Seq.forall (fun (a, b) -> a > b)
    let increasing = pairs |> Seq.forall (fun (a, b) -> a < b)

    decreasing || increasing

let isWithinRange min max list =
    let pairs = Seq.pairwise list
    let diffs = pairs |> Seq.map (fun (a, b) -> abs (a - b))

    diffs |> Seq.forall (fun x -> x >= min && x <= max)

let buildCandidates list =
    let indices = [ 0 .. (list |> Seq.length) - 1 ]
    let skipIndex i = Seq.append (list |> Seq.take i) (list |> Seq.skip i |> Seq.skip 1)

    Seq.append
        ([list] |> Seq.ofList)
        (indices |> Seq.map (fun i -> skipIndex i))

let safeReports =
    reports
        |> Seq.filter
            (fun list ->
                let cs = buildCandidates list

                cs |> Seq.exists (fun x -> (isMonotonic x) && (x |> isWithinRange 1 3)))

let numSafeReports = safeReports |> Seq.length

printfn "%d" numSafeReports
