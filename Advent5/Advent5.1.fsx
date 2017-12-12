let readLines φfilePath =
    let read = System.IO.File.ReadAllLines(φfilePath)
    read

let outOfBounds (φi : int) (φlen : int) =
    if φi < 0 || φlen <= φi then true else false

//Denne opgave klares bedst med
// muterbare arrays
let Advent5p1 (φfilePath) =
    let lines = readLines φfilePath
    let numbers = Array.map (fun x -> int x) lines
    let length = numbers.Length
    let mutable i = 0
    let mutable old = 0
    let mutable steps = 0
    while ((outOfBounds i length) = false) do
        old <- i
        i <- i + numbers.[i]
        numbers.[old] <- numbers.[old] + 1
        steps <- steps + 1
    i


Advent5p1 "Input.txt"

