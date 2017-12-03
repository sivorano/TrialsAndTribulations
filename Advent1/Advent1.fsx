 

let readRingIndex (φarr : 'a []) (φi : int) (φlen : int) =
    let newI = φi % φlen // We apply a modulus operator to move around a ring 
    φarr.[newI]

let readLines φfilePath =
    let read = System.IO.File.ReadAllLines(φfilePath)
    read.[0]

let charToInt x =
    match x with
    |'1' -> 1
    |'2' -> 2
    |'3' -> 3
    |'4' -> 4
    |'5' -> 5
    |'6' -> 6
    |'7' -> 7
    |'8' -> 8
    |'9' -> 9
    |'0' -> 0    
    |_ -> failwith "Not An Integer"
    
// We assume that the input has size >= 2
let Advent1 φfilePath =
    let input = (readLines φfilePath).ToCharArray()
    let len = input.Length
    let readRing x = readRingIndex input x len
    let mutable last = charToInt input.[0]
    let mutable current = charToInt input.[1]
    let mutable sum = 0
    for i = 1 to len do
        last <- charToInt (readRing (i - 1))
        current <- charToInt (readRing i)
        if last = current then
            sum <- sum + current
    sum
    
