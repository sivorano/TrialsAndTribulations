
let readLines φfilePath =
    let read = System.IO.File.ReadAllLines(φfilePath)
    read.[0]

let maxDiff (φstrxs : string []) =
    let intList = Array.map int φstrxs
    let minVal = Array.foldBack min intList intList.[0]
    let maxVal = Array.foldBack max intList intList.[0] 
    maxVal - minVal

let splitUpTab (φstr :string) =
    φstr.Split('\t')

let spreadSheetToLists φfilePath =
    let spread = readLines φfilePath
    let lines = spread.Split('\n') // We split on newlines, to get each line
    let diffs = Array.map (fun x -> maxDiff(splitUpTab x)) lines 
    printfn "%A" diffs

let k = "123 456 789
123 456 789"

k.Split('\n')

