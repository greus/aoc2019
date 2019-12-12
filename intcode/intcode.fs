module Intcode

let replaceAt index newVal (list:System.Collections.Generic.List<int>) =
    list.[index] <- newVal
    list

// let valAt index (list:System.Collections.Generic.List<int>) =
//     list.[index]

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

let getOperation opcode =
    let pad l = l @ (Array.create (4 - List.length l) "0" |> Array.toList)
    let toTuple (l:string list) = (l.[1] + l.[0] |> int, int l.[2], int l.[3])
    opcode |> string |> Seq.toList |> List.rev |> List.map (fun c -> c |> string) |> pad
    |> toTuple

let rec operate index input (list:System.Collections.Generic.List<int>) =
    let nextOperateAfterThreeParams = operate (index+4) 0
    let nextOperateAfterTwoParams = operate (index+3) 0

    let value i mode =
        match mode with
        | 0 -> list.[(list.[i])]
        | 1 -> list.[i]
        | _ -> failwith "input mode out of range"

    let operation = getOperation list.[index]
    match operation with
    | (1, imp1, imp2) -> list |> op1 (value (index+1) imp1) (value (index+2) imp2) (index+3) |> nextOperateAfterThreeParams
    | (2, imp1, imp2) -> list |> op2 (value (index+1) imp1) (value (index+2) imp2) (index+3) |> nextOperateAfterThreeParams
    | (3, _, _) -> list |> op3 (index+1) input |> operate (index+2) 0
    | (4, imp1, _) -> list |> operate (index+2) (value (index+1) imp1)
    | (5, imp1, imp2) -> list |> if (value (index+1) imp1) <> 0 then operate (value (index+2) imp2) 0 else nextOperateAfterTwoParams
    | (6, imp1, imp2) -> list |> if (value (index+1) imp1) = 0 then operate (value (index+2) imp2) 0 else nextOperateAfterTwoParams
    | (7, imp1, imp2) -> list |> op7 (value (index+1) imp1) (value (index+2) imp2) (index+3) |> nextOperateAfterThreeParams
    | (8, imp1, imp2) -> list |> op8 (value (index+1) imp1) (value (index+2) imp2) (index+3) |> nextOperateAfterThreeParams
    | (_, _, _) -> input

        // match list.[index] with
        // | 1 -> list |> op1 (valAt list.[(index+1)] list) (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
        // | 101 -> list |> op1 list.[(index+1)] (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
        // | 1001 -> list |> op1 (valAt list.[(index+1)] list) list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
        // | 1101 -> list |> op1 list.[(index+1)] list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
        // | 2 -> list |> op2 (valAt list.[(index+1)] list) (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
        // | 102 -> list |> op2 list.[(index+1)] (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
        // | 1002 -> list |> op2 (valAt list.[(index+1)] list) list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
        // | 1102 -> list |> op2 list.[(index+1)] list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
        // | 3 -> list |> op3 (index+1) input |> operate (index + 2) 0
        // | 4 -> list |> operate (index + 2) (valAt list.[(index+1)] list)
        // | 104 -> list |> operate (index + 2) list.[(index+1)]
        // | 5 -> list |> if (valAt list.[(index+1)] list) <> 0 then operate (valAt list.[(index+2)] list) 0  else operate (index+3) 0
        // | 105 -> list |> if list.[(index+1)] <> 0 then operate (valAt list.[(index+2)] list) 0  else operate (index+3) 0
        // | 1005 -> list |> if (valAt list.[(index+1)] list) <> 0 then operate list.[(index+2)] 0  else operate (index+3) 0
        // | 1105 -> list |> if list.[(index+1)] <> 0 then operate list.[(index+2)] 0  else operate (index+3) 0
        // | 6 -> list |> if (valAt list.[(index+1)] list) = 0 then operate (valAt list.[(index+2)] list) 0  else operate (index+3) 0
        // | 106 -> list |> if list.[(index+1)] = 0 then operate (valAt list.[(index+2)] list) 0  else operate (index+3) 0
        // | 1006 -> list |> if (valAt list.[(index+1)] list) = 0 then operate list.[(index+2)] 0  else operate (index+3) 0
        // | 1106 -> list |> if list.[(index+1)] = 0 then operate list.[(index+2)] 0  else operate (index+3) 0
        // | 7 -> list |> op7 (valAt list.[(index+1)] list) (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
        // | 107 -> list |> op7 list.[(index+1)] (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
        // | 1007 -> list |> op7 (valAt list.[(index+1)] list) list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
        // | 1107 -> list |> op7 list.[(index+1)] list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
        // | 8 -> list |> op8 (valAt list.[(index+1)] list) (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
        // | 108 -> list |> op8 list.[(index+1)] (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
        // | 1008 -> list |> op8 (valAt list.[(index+1)] list) list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
        // | 1108 -> list |> op8 list.[(index+1)] list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
        // | _ -> input

let intcode52 input (mutableList:System.Collections.Generic.List<int>) =
    mutableList |> operate 0 input

// https://stackoverflow.com/a/20308915
let stripchars chars str =
  Seq.fold
    (fun (str: string) chr ->
      str.Replace(chr |> System.Char.ToUpper |> string, "").Replace(chr |> System.Char.ToLower |> string, ""))
    str chars
let readText filePath = System.IO.File.ReadAllText(filePath) |> stripchars ['\n']
let splitLine = (fun (line : string) -> Seq.toList (line.Split ','))
let toInts list = list |> List.map (fun s -> int s)
let toMutable list = new System.Collections.Generic.List<int>(collection = list)
let toMutableInts = readText >> splitLine >> toInts >> toMutable

let test52 () =
    "input5"
    |> toMutableInts
    |> intcode52 5
    |> printfn "Expected: 2808771, Actual: %A"
