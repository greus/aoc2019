module Day5

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

let op1 v1 v2 si (list:System.Collections.Generic.List<int>) =
    list |> replaceAt list.[si] (v1 + v2)

let op2 v1 v2 si (list:System.Collections.Generic.List<int>) =
    list |> replaceAt list.[si] (v1 * v2)

let op3 si input (list:System.Collections.Generic.List<int>) =
    list |> replaceAt list.[si] input

let rec operate index input (list:System.Collections.Generic.List<int>) =
    let nextOperateAfter12 = operate (index + 4) 0
    match list.[index] with
    | 1 -> list |> op1 (valAt list.[(index+1)] list) (valAt list.[(index+2)] list) (index+3) |> nextOperateAfter12
    | 101 -> list |> op1 list.[(index+1)] (valAt list.[(index+2)] list) (index+3) |> nextOperateAfter12
    | 1001 -> list |> op1 (valAt list.[(index+1)] list) list.[(index+2)] (index+3) |> nextOperateAfter12
    | 1101 -> list |> op1 list.[(index+1)] list.[(index+2)] (index+3) |> nextOperateAfter12
    | 2 -> list |> op2 (valAt list.[(index+1)] list) (valAt list.[(index+2)] list) (index+3) |> nextOperateAfter12
    | 102 -> list |> op2 list.[(index+1)] (valAt list.[(index+2)] list) (index+3) |> nextOperateAfter12
    | 1002 -> list |> op2 (valAt list.[(index+1)] list) list.[(index+2)] (index+3) |> nextOperateAfter12
    | 1102 -> list |> op2 list.[(index+1)] list.[(index+2)] (index+3) |> nextOperateAfter12
    | 3 -> list |> op3 (index+1) input |> operate (index + 2) 0
    | 4 -> list |> operate (index + 2) (valAt list.[(index+1)] list)
    | 104 -> list |> operate (index + 2) list.[(index+1)]
    | _ -> input

let implPart1 input (mutableList:System.Collections.Generic.List<int>) =
    mutableList
    |> operate 0 input

let t1 () =
    let withInput1 = implPart1 1
    [
        new System.Collections.Generic.List<int>(collection = [3;0;4;0;99]); // = 1
        new System.Collections.Generic.List<int>(collection = [104;1337;99]) // = 1337
    ]
    |> List.map withInput1

let p1 () =
    readText "input"
    |> splitLine |> toInts |> toMutable
    |> implPart1 1
