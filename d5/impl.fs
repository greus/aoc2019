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

let op7 v1 v2 si (list:System.Collections.Generic.List<int>) =
    list |> replaceAt list.[si] (if v1 < v2 then 1 else 0)

let op8 v1 v2 si (list:System.Collections.Generic.List<int>) =
    list |> replaceAt list.[si] (if v1 = v2 then 1 else 0)

let rec operate index input (list:System.Collections.Generic.List<int>) =
    let nextOperateAfterThreeParams = operate (index + 4) 0
    match list.[index] with
    | 1 -> list |> op1 (valAt list.[(index+1)] list) (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
    | 101 -> list |> op1 list.[(index+1)] (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
    | 1001 -> list |> op1 (valAt list.[(index+1)] list) list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
    | 1101 -> list |> op1 list.[(index+1)] list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
    | 2 -> list |> op2 (valAt list.[(index+1)] list) (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
    | 102 -> list |> op2 list.[(index+1)] (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
    | 1002 -> list |> op2 (valAt list.[(index+1)] list) list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
    | 1102 -> list |> op2 list.[(index+1)] list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
    | 3 -> list |> op3 (index+1) input |> operate (index + 2) 0
    | 4 -> list |> operate (index + 2) (valAt list.[(index+1)] list)
    | 104 -> list |> operate (index + 2) list.[(index+1)]
    | 5 -> list |> if (valAt list.[(index+1)] list) <> 0 then operate (valAt list.[(index+2)] list) 0  else operate (index+3) 0
    | 105 -> list |> if list.[(index+1)] <> 0 then operate (valAt list.[(index+2)] list) 0  else operate (index+3) 0
    | 1005 -> list |> if (valAt list.[(index+1)] list) <> 0 then operate list.[(index+2)] 0  else operate (index+3) 0
    | 1105 -> list |> if list.[(index+1)] <> 0 then operate list.[(index+2)] 0  else operate (index+3) 0
    | 6 -> list |> if (valAt list.[(index+1)] list) = 0 then operate (valAt list.[(index+2)] list) 0  else operate (index+3) 0
    | 106 -> list |> if list.[(index+1)] = 0 then operate (valAt list.[(index+2)] list) 0  else operate (index+3) 0
    | 1006 -> list |> if (valAt list.[(index+1)] list) = 0 then operate list.[(index+2)] 0  else operate (index+3) 0
    | 1106 -> list |> if list.[(index+1)] = 0 then operate list.[(index+2)] 0  else operate (index+3) 0
    | 7 -> list |> op7 (valAt list.[(index+1)] list) (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
    | 107 -> list |> op7 list.[(index+1)] (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
    | 1007 -> list |> op7 (valAt list.[(index+1)] list) list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
    | 1107 -> list |> op7 list.[(index+1)] list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
    | 8 -> list |> op8 (valAt list.[(index+1)] list) (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
    | 108 -> list |> op8 list.[(index+1)] (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
    | 1008 -> list |> op8 (valAt list.[(index+1)] list) list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
    | 1108 -> list |> op8 list.[(index+1)] list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
    | _ -> input

let implPart2 input (mutableList:System.Collections.Generic.List<int>) =
    mutableList
    |> operate 0 input

let t2 input =
    let withInput = implPart2 input
    [
        // p mode, = 8 -> 1
        new System.Collections.Generic.List<int>(collection = [3;9;8;9;10;9;4;9;99;-1;8]);
    ]
    |> List.map withInput

let p2 () =
    readText "input"
    |> splitLine |> toInts |> toMutable
    |> implPart2 5
