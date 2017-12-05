
let readLines φfilePath =
    let read = System.IO.File.ReadAllLines(φfilePath)
    read

let maxDiff (φstrxs : string []) =
    let intList = Array.map int φstrxs
    let minVal = Array.foldBack min intList intList.[0]
    let maxVal = Array.foldBack max intList intList.[0] 
    maxVal - minVal

let splitUpTab (φstr :string) =
    φstr.Split('\t')

let Advent2p1 φfilePath =
    let spread = readLines φfilePath
    let diffs = Array.map (fun x -> maxDiff(splitUpTab x)) spread 
    Array.foldBack (+) diffs 0


printfn "Checksum: %i" (Advent2p1 "Intput1.txt")

