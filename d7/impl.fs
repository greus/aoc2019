module Day7

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

let rec operate index input1 input2 (list:System.Collections.Generic.List<int>) =
    let nextOperateAfterThreeParams = operate (index + 4) input1 0
    match list.[index] with
    | 1 -> list |> op1 (valAt list.[(index+1)] list) (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
    | 101 -> list |> op1 list.[(index+1)] (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
    | 1001 -> list |> op1 (valAt list.[(index+1)] list) list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
    | 1101 -> list |> op1 list.[(index+1)] list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
    | 2 -> list |> op2 (valAt list.[(index+1)] list) (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
    | 102 -> list |> op2 list.[(index+1)] (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
    | 1002 -> list |> op2 (valAt list.[(index+1)] list) list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
    | 1102 -> list |> op2 list.[(index+1)] list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
    | 3 -> list |> op3 (index+1) input1 |> operate (index + 2) input2 0
    | 4 -> list |> operate (index + 2) (valAt list.[(index+1)] list) 0
    | 104 -> list |> operate (index + 2) list.[(index+1)] 0
    | 5 -> list |> if (valAt list.[(index+1)] list) <> 0 then operate (valAt list.[(index+2)] list) input1 0 else operate (index+3) input1 0
    | 105 -> list |> if list.[(index+1)] <> 0 then operate (valAt list.[(index+2)] list) input1 0  else operate (index+3) input1 0
    | 1005 -> list |> if (valAt list.[(index+1)] list) <> 0 then operate list.[(index+2)] input1 0  else operate (index+3) input1 0
    | 1105 -> list |> if list.[(index+1)] <> 0 then operate list.[(index+2)] input1 0  else operate (index+3) input1 0
    | 6 -> list |> if (valAt list.[(index+1)] list) = 0 then operate (valAt list.[(index+2)] list) input1 0 else operate (index+3) input1 0
    | 106 -> list |> if list.[(index+1)] = 0 then operate (valAt list.[(index+2)] list) input1 0 else operate (index+3) input1 0
    | 1006 -> list |> if (valAt list.[(index+1)] list) = 0 then operate list.[(index+2)] input1 0 else operate (index+3) input1 0
    | 1106 -> list |> if list.[(index+1)] = 0 then operate list.[(index+2)] input1 0 else operate (index+3) input1 0
    | 7 -> list |> op7 (valAt list.[(index+1)] list) (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
    | 107 -> list |> op7 list.[(index+1)] (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
    | 1007 -> list |> op7 (valAt list.[(index+1)] list) list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
    | 1107 -> list |> op7 list.[(index+1)] list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
    | 8 -> list |> op8 (valAt list.[(index+1)] list) (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
    | 108 -> list |> op8 list.[(index+1)] (valAt list.[(index+2)] list) (index+3) |> nextOperateAfterThreeParams
    | 1008 -> list |> op8 (valAt list.[(index+1)] list) list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
    | 1108 -> list |> op8 list.[(index+1)] list.[(index+2)] (index+3) |> nextOperateAfterThreeParams
    | _ -> input1

let t1 () =
    let list = [3;15;3;16;1002;16;10;16;1;16;15;15;4;15;99;0;0]

    // Max thruster signal 43210 (from phase setting sequence 4,3,2,1,0)
    let i1 = list |> toMutable |> operate 0 4 0
    printfn "%A" i1

    let i2 = list |> toMutable |> operate 0 3 i1
    printfn "%A" i2

    let i3 = list |> toMutable |> operate 0 2 i2
    printfn "%A" i3

    let i4 = list |> toMutable |> operate 0 1 i3
    printfn "%A" i4

    let i5 = list |> toMutable |> operate 0 0 i4
    printfn "%A" i5

// http://www.fssnip.net/6C/title/Permutation-and-Combination
type ListBuilder() =
  let concatMap f m = List.concat( List.map (fun x -> f x) m )
  member this.Bind (m, f) = concatMap (fun x -> f x) m
  member this.Return (x) = [x]
  member this.ReturnFrom (x) = x
  member this.Zero () = []
  member this.Combine (a,b) = a@b
  member this.Delay f = f ()

let list = ListBuilder()

let rec permutations n lst =
  let rec selections = function
      | []      -> []
      | x::xs -> (x,xs) :: list { let! y,ys = selections xs
                                  return y,x::ys }
  (n, lst) |> function
  | 0, _ -> [[]]
  | _, [] -> []
  | _, x::[] -> [[x]]
  | n, xs -> list { let! y,ys = selections xs
                    let! zs = permutations (n-1) ys
                    return y::zs }

let output (phases: int list) acs =
    // printfn "%A" phases
    let p1 = phases.[0]
    let p2 = phases.[1]
    let p3 = phases.[2]
    let p4 = phases.[3]
    let p5 = phases.[4]
    let i1 = acs |> toMutable |> operate 0 p1 0
    let i2 = acs |> toMutable |> operate 0 p2 i1
    let i3 = acs |> toMutable |> operate 0 p3 i2
    let i4 = acs |> toMutable |> operate 0 p4 i3
    let i5 = acs |> toMutable |> operate 0 p5 i4
    [i1;i2;i3;i4;i5]
    |> List.sort
    |> List.rev
    |> List.head

let p1 () =
    let acs = readText "input" |> splitLine |> toInts

    [0..4] |> permutations 5
    |> List.map (fun phases -> output phases acs)
    |> List.sort
    |> List.rev
    |> List.head
