
let readLines φfilePath =
    let read = System.IO.File.ReadAllLines(φfilePath)
    read

let spreadOut (φinxs :int list) i =
    let xs = List.toArray φinxs
    let value = xs.[i]
    let len = xs.Length
    xs.[i] <- 0
    //the problem with 0 is not a problem in this case
    for j= i + 1 to i + value do
        xs.[j % len] <- xs.[j % len] + 1
    Array.toList xs

let findMaxIndex (φinxs : int list) =
    let maks = List.foldBack max φinxs 0
    List.findIndex (fun x -> x = maks) φinxs;;

let rec findRepeat
    

