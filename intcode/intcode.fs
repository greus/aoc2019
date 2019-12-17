module Intcode

let zero = 0L

let replaceAt index newVal (list:int64 array) =
    list.[index] <- newVal
    list

let op1 v1 v2 (si:int) (list:int64 array) =
    list |> replaceAt si (v1 + v2)

let op2 v1 v2 (si:int) (list:int64 array) =
    list |> replaceAt si (v1 * v2)

let op3 (si:int) input (list:int64 array) =
    list |> replaceAt si input

let op7 v1 v2 (si:int) (list:int64 array) =
    list |> replaceAt si (if v1 < v2 then 1L else 0L)

let op8 v1 v2 (si:int) (list:int64 array) =
    list |> replaceAt si (if v1 = v2 then 1L else 0L)

let getOperation opcode =
    let pad l = l @ (Array.create (5 - List.length l) "0" |> Array.toList)
    let toTuple (l:string list) = (l.[1] + l.[0] |> int, int l.[2], int l.[3], int l.[4])
    opcode |> string |> Seq.toList |> List.rev |> List.map (fun c -> c |> string) |> pad
    |> toTuple

let rec operate (rb:int) (index:int) (input:int64) (list:int64 array) =
    let nextOperateAfterThreeParams = operate rb (index+4) input
    let nextOperateAfterTwoParams = operate rb (index+3) input

    let value (i:int) mode =
        match mode with
        | 0 -> list.[int (list.[i])]
        | 1 -> list.[i]
        | 2 -> list.[(rb + int list.[i])]
        | _ -> failwith "input mode out of range"

    let posIndex i mode = if mode = 2 then rb + (int list.[i]) else int list.[i]

    // printfn "%d, %d, (%d, %d, %d)" list.[index] rb list.[(index+1)] list.[(index+2)] list.[(index+3)]
    let operation = getOperation list.[index]
    match operation with
    | (1, imp1, imp2, imp3) -> list |> op1 (value (index+1) imp1) (value (index+2) imp2) (posIndex (index+3) imp3) |> nextOperateAfterThreeParams
    | (2, imp1, imp2, imp3) -> list |> op2 (value (index+1) imp1) (value (index+2) imp2) (posIndex (index+3) imp3) |> nextOperateAfterThreeParams
    | (3, imp1, _, _) -> list |> op3 (posIndex (index+1) imp1) input |> operate rb (index+2) input
    | (4, imp1, _, _) -> value (index+1) imp1
    | (5, imp1, imp2, _) -> list |> if (value (index+1) imp1) <> zero then operate rb (value (index+2) imp2 |> int) input else nextOperateAfterTwoParams
    | (6, imp1, imp2, _) -> list |> if (value (index+1) imp1) = zero then operate rb (value (index+2) imp2 |> int) input else nextOperateAfterTwoParams
    | (7, imp1, imp2, imp3) -> list |> op7 (value (index+1) imp1) (value (index+2) imp2) (posIndex (index+3) imp3) |> nextOperateAfterThreeParams
    | (8, imp1, imp2, imp3) -> list |> op8 (value (index+1) imp1) (value (index+2) imp2) (posIndex (index+3) imp3) |> nextOperateAfterThreeParams
    | (9, imp1, _, _) ->
        let rbChange = value (index+1) imp1 |> int
        list |> operate (rb+rbChange) (index+2) input
    | (_, _, _, _) -> input

let intcode52 input (mutableList:int64 array) =
    mutableList |> operate 0 0 input

// https://stackoverflow.com/a/20308915
let stripchars chars str =
  Seq.fold
    (fun (str: string) chr ->
      str.Replace(chr |> System.Char.ToUpper |> string, "").Replace(chr |> System.Char.ToLower |> string, ""))
    str chars
let readText filePath = System.IO.File.ReadAllText(filePath) |> stripchars ['\n']
let splitLine = (fun (line : string) -> Seq.toList (line.Split ','))
let toInts list = list |> List.map (fun s -> int64 s)
let toMutable list = list |> List.toArray
let padArray max arr =
    Array.append arr (Array.create (max - Array.length arr) 0L)
let toMutableInts = splitLine >> toInts >> toMutable
let toFatMutableInts max =
    let padMax = padArray max
    splitLine >> toInts >> toMutable >> padMax

let test52 () =
    readText "input5"
    |> toMutableInts
    |> intcode52 5L
    |> printfn "Expected: 2808771, Actual: %d"

let testInt64 () =
    "104,1125899906842624,99"
    |> toMutableInts
    |> operate 0 0 0L
    |> printfn "Expected: 1125899906842624, Actual: %d"

let test91 () =
    let max = 2147483647 / 100000

    readText "input9"
    |> toFatMutableInts max
    |> operate 0 0 1L
    |> printfn "Expected: 2204990589, Actual: %d"

let test92 () =
    let max = 2147483647 / 100000

    readText "input9"
    |> toFatMutableInts max
    |> operate 0 0 2L
    |> printfn "Expected: 50008, Actual: %d"
