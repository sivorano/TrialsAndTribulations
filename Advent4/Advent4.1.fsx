 
let readLines φfilePath =
    let read = System.IO.File.ReadAllLines(φfilePath)
    read


//Checks if a string contains duplicate words
let duplicateWords (φstr : string) =
    let words = φstr.Split(' ')
    let sorted = Array.sort(words)
    // why not call a lambda expression λ
    let λ = (fun x (y,z) -> if x=y then (x,false)
                                   else (x,z))  
    let (ls,out) = Array.foldBack λ  sorted ("",true)
    out

let Advent4p1 φfilePath =
    let lines = readLines φfilePath
    let cont = Array.map duplicateWords lines
    let λ = (fun x y -> if x = true then y + 1 else y)
    Array.foldBack λ cont 0
    

