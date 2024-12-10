// Upon completion, two things immediately become clear. First, the disk definitely has a lot more contiguous free space, just like the amphipod hoped. Second, the computer is running much more slowly! Maybe introducing all of that file system fragmentation was a bad idea?
//
// The eager amphipod already has a new plan: rather than move individual blocks, he'd like to try compacting the files on his disk by moving whole files instead.
//
// This time, attempt to move whole files to the leftmost span of free space blocks that could fit the file. Attempt to move each file exactly once in order of decreasing file ID number starting with the file with the highest file ID number. If there is no span of free space to the left of a file that is large enough to fit the file, the file does not move.
//
// The first example from above now proceeds differently:
//
// 00...111...2...333.44.5555.6666.777.888899
// 0099.111...2...333.44.5555.6666.777.8888..
// 0099.1117772...333.44.5555.6666.....8888..
// 0099.111777244.333....5555.6666.....8888..
// 00992111777.44.333....5555.6666.....8888..
//
// The process of updating the filesystem checksum is the same; now, this example's checksum would be 2858.
//
// Start over, now compacting the amphipod's hard drive using this new method instead. What is the resulting filesystem checksum?

open System
open System.IO

type DiskBlock =
    | File of int
    | FreeSpace
    | Empty

type Section = {
    Block: DiskBlock
    Index: int
    Length: int
}

let rawDiskMap = File.ReadAllText "../day-09-part-1/input.txt"

// let rawDiskMap = "2333133121414131402"

let asDigit (x: char) = int x - int '0'

let diskMap =
    rawDiskMap + " "
        |> Seq.chunkBySize 2
        |> Seq.mapi (fun i xs ->
            match xs with
            | [| file; freesize|] -> (i, asDigit file, if freesize = ' ' then None else Some (asDigit freesize))
            | _ -> raise <| InvalidOperationException "Unexpected input")
        |> Seq.collect (fun (i, file, freesize) ->
            let fileBlocks = [ for _ in 1..file -> File i ]
            let freeBlocks =
                match freesize with
                | Some fs -> [ for _ in 1..fs -> FreeSpace ]
                | None -> []
            fileBlocks @ freeBlocks)
        |> Array.ofSeq

let partitionBlocks (map: DiskBlock array) =
    let blocks = ResizeArray<Section>()
    let (x, xs) = (Seq.head map, Seq.tail map)
    let folder (index: int, starting: int, last: DiskBlock, blocks: ResizeArray<Section>) (current: DiskBlock) =
        if current <> last then
            blocks.Add { Block = last; Index = starting; Length = index - starting }
            (index + 1, index, current, blocks)
        else
            (index + 1, starting, last, blocks)

    let (index, starting, last, results) = xs |> Seq.fold folder (1, 0, x, blocks)

    results.Add({ Block = last; Index = starting; Length = index - starting })

    results |> List.ofSeq

let compactDiskMap (diskMap: DiskBlock array) =
    let partitions = partitionBlocks diskMap
    let mutable freeSpaces = partitions |> List.filter (fun x -> x.Block = FreeSpace)
    let files = partitions |> List.filter (fun x -> x.Block <> FreeSpace)
    let fillBlock destIndex section =
        let rec innerFill index length =
            if length > 0 then
                diskMap[index] <- section.Block
                innerFill (index + 1) (length - 1)

        innerFill destIndex section.Length

    files
        |> List.tail
        |> List.rev
        |> List.iter
            (fun file ->
                let freeSpace = freeSpaces |> List.tryFind (fun x -> x.Length >= file.Length && file.Index > x.Index)

                match freeSpace with
                | Some fs ->
                    fillBlock fs.Index file
                    fillBlock file.Index { file with Block = FreeSpace }

                    if fs.Length > file.Length then
                        let newFreeSpaceBlock = { Block = FreeSpace; Index = fs.Index + file.Length; Length = fs.Length - file.Length }
                        freeSpaces <- (newFreeSpaceBlock :: (freeSpaces |> List.filter (fun x -> x <> fs))) |> List.sortBy _.Index
                    elif fs.Length = file.Length then
                        freeSpaces <- freeSpaces |> List.filter (fun x -> x <> fs)
                | None -> ())

    diskMap |> Array.filter (fun x -> x <> Empty)

let compactedDiskMap = compactDiskMap diskMap

let checksum =
    compactedDiskMap
    |> Array.mapi (fun i x -> (int64 i) * (match x with File id -> (int64 id) | _ -> 0))
    |> Array.sum

printfn "%A" checksum
