module Day1

open System

let readLines filePath = System.IO.File.ReadLines(filePath)

let reqFuel mass =
    floor (decimal mass / 3m) - 2m
    |> int

let t1 () =
    reqFuel 100756
    |> printfn "%i"

let p1 () =
    readLines "input"
    |> Seq.toList
    |> List.map (fun s -> int s |> reqFuel)
    |> List.sum
    |> printfn "%i"

let rec recFuel fuel =
    match fuel with
    | f when f <=0 -> 0
    | _ -> fuel + recFuel (reqFuel fuel)

let reqFuel2 mass =
    mass |> reqFuel |> recFuel

let p2impl masses =
    masses
    |> List.map (fun s -> int s |> reqFuel2)
    |> List.sum
    |> printfn "%i"

let t2 () =
    ["1969"]
    |> p2impl

let p2 () =
    readLines "input"
    |> Seq.toList
    |> p2impl
