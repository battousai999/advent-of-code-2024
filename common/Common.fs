module Common

open System
open System.Text.RegularExpressions

let (|Regexer|_|) (regex : Regex) input =
    let m = regex.Match(input)

    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let buildMap<'a> (defaultElement: 'a) (selector: (char -> 'a)) (rawMap: string array) =
    let xSize = rawMap |> Array.head |> Seq.length
    let ySize = rawMap |> Array.length
    let map = Array2D.create xSize ySize defaultElement

    rawMap
        |> Array.iteri
            (fun yIndex line ->
                line
                    |> Seq.iteri
                        (fun xIndex ch ->
                            let value = selector ch
                            map[xIndex, yIndex] <- value))

    map

let identity x = x

let toSeq<'a> (arr: 'a array2d) =
    let length1 = arr |> Array2D.length1
    let length2 = arr |> Array2D.length2
    seq {
        for i in 0 .. length1-1 do
        for j in 0 .. length2-1 do
        yield arr[i,j]
    }

let infiniteSeq () =
    seq {
        let mutable i = 0L

        while true do
            if i < Int64.MaxValue then
                i <- i + 1L

            yield i
    }

let isEven n = n % 2 = 0
let isOdd n = n % 2 = 1
