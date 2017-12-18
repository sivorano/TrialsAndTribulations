let readLines φfilePath =
    let read = System.IO.File.ReadAllLines(φfilePath)
    read

let anagram (φstr : string) =
    let chars = φstr.ToCharArray()
    let rearanged = Array.sort(chars)
    string (System.String rearanged)

//Checks if a string contains duplicate words
let duplicateAnagram (φstr : string) =
    let words = Array.map anagram (φstr.Split(' '))
    let sorted = Array.sort(words)
    // why not call a lambda expression λ
    let λ = (fun x (y,z) -> if x=y then (x,false)
                                   else (x,z))  
    let (ls,out) = Array.foldBack λ  sorted ("",true)
    out

let Advent4p2 φfilePath =
    let lines = readLines φfilePath
    let cont = Array.map duplicateAnagram lines
    let λ = (fun x y -> if x = true then y + 1 else y)
    Array.foldBack λ cont 0
    

