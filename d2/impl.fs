module Day2

open System

// https://stackoverflow.com/a/20308915
let stripchars chars str =
  Seq.fold
    (fun (str: string) chr ->
      str.Replace(chr |> Char.ToUpper |> string, "").Replace(chr |> Char.ToLower |> string, ""))
    str chars

let readText filePath = System.IO.File.ReadAllText(filePath) |> stripchars ['\n']

let splitLine = (fun (line : string) -> Seq.toList (line.Split ','))

let toInts list = list |> List.map (fun s -> int s)

let toMutable list = new System.Collections.Generic.List<int>(collection = list)

let replaceAt index newVal (list:System.Collections.Generic.List<int>) =
    list.[index] <- newVal
    list

let valAt index (list:System.Collections.Generic.List<int>) =
    list.[index]

let op1 vi1 vi2 si (list:System.Collections.Generic.List<int>) =
    list |> replaceAt list.[si] ((valAt list.[vi1] list) + (valAt list.[vi2] list))

let op2 vi1 vi2 si (list:System.Collections.Generic.List<int>) =
    list |> replaceAt list.[si] ((valAt list.[vi1] list) * (valAt list.[vi2] list))

let rec operate index (list:System.Collections.Generic.List<int>) =
    match list.[index] with
    | 1 -> list |> op1 (index+1) (index+2) (index+3) |> operate (index + 4)
    | 2 -> list |> op2 (index+1) (index+2) (index+3) |> operate (index + 4)
    | _ -> list

let p1impl (mutableList:System.Collections.Generic.List<int>) =
    mutableList
    |> operate 0

let t1 () =
    // new System.Collections.Generic.List<int>(collection = [1;0;0;0;99])
    new System.Collections.Generic.List<int>(collection = [2;4;4;5;99;0])
    |> p1impl

let p1 () =
    readText "input"
    |> splitLine |> toInts |> toMutable
    |> replaceAt 1 12
    |> replaceAt 2 2
    |> p1impl

let p2impl noun verb initalList =
    let output = (initalList |> toMutable |> replaceAt 1 noun |> replaceAt 2 verb |> p1impl)
    output.[0]

let rec findMatch combos initalList =
    match combos with
    | [] -> (0, 0)
    | (v, n)::tail ->
        match p2impl v n initalList with
         | 19690720 -> (v, n)
         | _ -> findMatch tail initalList

let p2 () =
    let initalList = readText "input" |> splitLine |> toInts
    let combos = [for n in 0..99 do for v in 0..99 do yield (n, v)]

    findMatch combos initalList
