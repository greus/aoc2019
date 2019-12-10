module Day8

open System

// https://stackoverflow.com/a/20308915
let stripchars chars str =
  Seq.fold
    (fun (str: string) chr ->
      str.Replace(chr |> Char.ToUpper |> string, "").Replace(chr |> Char.ToLower |> string, ""))
    str chars
let readText filePath = System.IO.File.ReadAllText(filePath) |> stripchars ['\n']
let splitLine = (fun (line : string) -> Seq.toList line)
let toInts list = list |> List.map (fun c -> string c |> int)
let toDigits = readText >> splitLine >> toInts

let toLayers size digits =
    List.chunkBySize size digits

let part1 wpx hpx digits =
    let zeros elem = if elem = 0 then 1 else 0
    let onesByTwos layer =
        let ones = layer |> List.filter (fun d -> d = 1) |> List.length
        let twos = layer |> List.filter (fun d -> d = 2) |> List.length
        ones * twos

    toLayers (wpx*hpx) digits
    |> List.map (fun l -> (l |> List.countBy zeros |> List.rev |> List.head |> snd, l))
    |> List.sortBy (fun (zeros, _) -> zeros)
    |> List.head
    |> snd
    |> onesByTwos

let t1 () =
    [1;0;0;2;1;6;1;2;1;1;2;0]
    |> part1 3 2

let p1 () =
    "input" |> toDigits |> part1 25 6

let rec flatten (acc:System.Collections.Generic.List<int>) layers =
    let foo l r =
        for i = 0 to (List.length l - 1) do
            if [0;1] |> List.contains l.[i] then acc.[i] <- l.[i]
        flatten acc r

    match layers with
    | [] -> acc
    | layer::rest -> foo layer rest


let part2 wpx hpx digits =
    let draw row =
        row |> Seq.iter (fun d -> if d = 1 then printf "O" else printf " ")
        printfn ""

    toLayers (wpx*hpx) digits
    |> List.rev
    |> flatten (new System.Collections.Generic.List<int>(collection=Array.create (wpx*hpx) 2))
    |> Seq.chunkBySize wpx
    |> Seq.iter draw

let t2 () =
    [0;2;2;2;1;1;2;2;2;2;1;2;0;0;0;0]
    |> part2 2 2 // 01\n10

let p2 () =
    "input" |> toDigits |> part2 25 6
