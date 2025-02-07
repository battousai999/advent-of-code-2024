// The staff don't really like some of the towel arrangements you came up with. To avoid an endless cycle of towel
// rearrangement, maybe you should just give them every possible option.
//
// Here are all of the different ways the above example's designs can be made:
//
// brwrr can be made in two different ways: b, r, wr, r or br, wr, r.
//
// bggr can only be made with b, g, g, and r.
//
// gbbr can be made 4 different ways:
//
// g, b, b, r
// g, b, br
// gb, b, r
// gb, br
//
// rrbgbr can be made 6 different ways:
//
// r, r, b, g, b, r
// r, r, b, g, br
// r, r, b, gb, r
// r, rb, g, b, r
// r, rb, g, br
// r, rb, gb, r
//
// bwurrg can only be made with bwu, r, r, and g.
//
// brgr can be made in two different ways: b, r, g, r or br, g, r.
//
// ubwu and bbrgwb are still impossible.
//
// Adding up all of the ways the towels in this example could be arranged into the desired designs yields
// 16 (2 + 1 + 4 + 6 + 1 + 2).
//
// They'll let you into the onsen as soon as you have the list. What do you get if you add up the number of different
// ways you could make each design?

open System
open System.IO
open Battousai.Utils.StringUtils
open Common

let rawInput = File.ReadAllLines("../day-19-part-1/input.txt")

// let rawInputStr = @"r, wr, b, g, bwu, rb, gb, br
//
// brwrr
// bggr
// gbbr
// rrbgbr
// ubwu
// bwurrg
// brgr
// bbrgwb"

// let rawInput = rawInputStr.Split(Environment.NewLine)

let patterns =
    rawInput[0].Split(',')
    |> Array.map (fun x -> x.Trim())
    |> List.ofArray

let designs =
    rawInput
    |> Array.skip 2
    |> List.ofArray

let getPossibleDesigns = memoizeRecursiveFunction <| (fun recursiveFunc ((patterns, design): ((string list) * string)) ->
    if design = String.Empty then
        1L
    else
        let matches = patterns |> List.filter (fun p -> design.StartsWith(p))

        matches
        |> List.sumBy
            (fun m ->
                let rest = design.RemoveLeading(m)

                recursiveFunc (patterns, rest)))

let sumPossible =
    designs
    |> List.sumBy (fun design -> getPossibleDesigns (patterns, design))

printfn "%d" sumPossible
