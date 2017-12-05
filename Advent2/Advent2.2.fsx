
let readLines φfilePath =
    let read = System.IO.File.ReadAllLines(φfilePath)
    read


let evenlyDivide x y =
    if ((x % y) = 0) || ((y % x) = 0) then true
                                      else false


let findEvenlyDivide (φintxs : int []) =
    let evenDivide (x :int) (arr : int [] ) =
           Array.foldBack (fun z y -> if z % x = 0 && x <> z then z else y) arr -1
    // A slightly to large λ function       
    Array.foldBack (fun x z -> if (evenDivide x φintxs) = -1
                                    then z
                                    else (evenDivide x φintxs,x)) φintxs (-1,-1)

let divide (x,y) = x / y
    
let maxDiffEven (φstrxs : string []) =
    let intList = Array.map int φstrxs
    let div = findEvenlyDivide intList
    divide div

let splitUpTab (φstr :string) =
    φstr.Split('\t')


let Advent2p2 φfilePath =
    let spread = readLines φfilePath
    let diffs = Array.map (fun x -> maxDiffEven(splitUpTab x)) spread
    Array.foldBack (+) diffs 0


printfn "Checksum: %i" (Advent2p2 "Input1.txt")

