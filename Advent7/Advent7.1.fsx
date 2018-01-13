let readLines φfilePath =
    let read = System.IO.File.ReadAllLines(φfilePath)
    read

let stringParser (φstr : string) =
    let first = (φstr.Split '(').[0].Trim()
    if (φstr.Split '>').Length = 1
        then (first,[|""|])
        else
             let other = ((φstr.Split '>').[1].Split ','
             |> Array.map (fun x -> x.Trim()))
             (first, other)

let isIn x (y,ys) =
    Array.contains x ys

let first (x,y) = x

let Advent7p1 (φinput : string []) =
  let parsed = Array.map stringParser φinput
  let len = parsed.Length
  let rec findIn = function
    |(s,i,xs) when i = len - 1 -> s
    |(s,i,xs : (string * (string [])) []) when isIn s xs.[i]
      -> findIn(first xs.[i],0, xs)
    |(s,i,xs) -> findIn(s,i + 1, xs)
  findIn (first parsed.[0],0,parsed)


