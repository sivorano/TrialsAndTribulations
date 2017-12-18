
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

let Advent6p2 (φfilepath) =
    let input = (readLines φfilepath) |> Array.toList |> List.map int
    let rec findRepeat = function
        |((xs : int list list),i,x) when List.contains x xs
            -> (List.findIndex (fun y -> y=x) xs) + 1
        |(xs,i,x) -> findRepeat(x :: xs,i + 1, spreadOut x (findMaxIndex(x)))
    findRepeat ([],0,input)

