module Day3

open System

let readLines filePath = System.IO.File.ReadLines(filePath)
let splitLine = (fun (line : string) -> Seq.toList (line.Split ','))

// https://stackoverflow.com/a/3722671
let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some (int (s.Substring(p.Length)))
    else
        None

let getChange instruction =
    match instruction with
    | Prefix "R" steps -> (steps, 0) // printfn "Right %A" steps
    | Prefix "D" steps -> (0, -steps)  // printfn "Down %A" steps
    | Prefix "L" steps -> (-steps, 0) // printfn "Left %A" steps
    | Prefix "U" steps -> (0, steps) // printfn "Up %A" steps
    | _ -> (0, 0) // printfn "Bad step"

let toCoord (coords, (px, py)) instruction =
    let (cx, cy) = getChange instruction
    let newPos = (px + cx, py + cy)
    let (nx, ny) = newPos
    let xCoords = if px = nx then [] elif px > nx then [for x in nx..px -> (x, ny)] else [for x in px..nx -> (x, ny)]
    let yCoords = if py = ny then [] elif py > ny then [for y in ny..py -> (nx, y)] else [for y in py..ny -> (nx, y)]

    (xCoords @ yCoords @ coords, newPos)

let toCoords line =
    line
    |> List.fold toCoord ([],(0,0))
    |> fst

let findIntersections lines =
    match lines with
    |l1::l2::[] -> l1 |> List.filter (fun e1 -> (l2 |> List.exists (fun e2 -> e1 = e2)))

let withClosestDistance intersections =
    let manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
    let central = (0, 0)
    intersections
    |> List.map (fun p -> manhattanDistance central p)
    // |> List.filter (fun d -> d > 0)
    |> List.sort
    // |> List.head

let implPart1 lines =
    lines
    |> List.map toCoords
    |> findIntersections
    |> withClosestDistance

let t1 () =
    // [["R75";"D30";"R83";"U83";"L12";"D49";"R71";"U7";"L72"];["U62";"R66";"U55";"R34";"D71";"R55";"D58";"R83"]]
    [["R8";"U5";"L5";"D3"];["U7";"R6";"D4";"L4"]]
    |> implPart1

let p1 () =
    readLines "input" |> Seq.toList |> List.map splitLine
    |> implPart1
