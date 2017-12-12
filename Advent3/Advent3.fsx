
let input = 289326

//We find the layer of the number
let layer φinx =
    let rec inlayer = function
        |(n,l,s) when n <= s -> (l,s)
        |(n,l,s) -> inlayer (n,l + 1,s + (2 * l) * 4)
    inlayer (φinx,1,1)

//The d1 metric on the real numbers
let d1 (x,y) = abs(x) + abs(y)

//We travel back to find the distance
let travel φin =
    let (x,y) = layer φin
    let rec traverse = function
        |(n,s,l,sm,px,py) when sm = n -> (px,py)
        |(n,s,l,sm,px,py) when px = l  && py > -l -> traverse(n,s,l,sm - 1, px + 1 , py)
        |(n,s,l,sm,px,py) when px < l && py = l ->  traverse(n,s,l,sm - 1, px + 1 , py)
        |(n,s,l,sm,px,py) when px = -l && py < l ->  traverse(n,s,l,sm - 1, px , py + 1)
        |(n,s,l,sm,px,py) when px > -l && py = -l -> traverse(n,s,l,sm - 1, px - 1, py)
        |_ -> failwith "We succcesfully failed"
    traverse (φin,y,x - 1,y,x - 1,-x + 1)


printfn "%i" (d1(travel input))






