module Day7p2

// #load "../intcode/intcode.fs";;
open Intcode

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

let highestSignal (phases: int64 list) acs =
    let toAmp phase =
        let amp = acs |> toMutable
        amp |> op3 (int amp.[1]) phase

    let amps = phases |> List.map toAmp
    let indexes = Array.create (Seq.length amps) 2

    let rec loop iteration (signal, code) isDone next =
        if isDone iteration code
            then
                signal
            else
                let (s, c, i) = operate 0 indexes.[iteration] signal (amps.[iteration])
                indexes.[iteration] <- i
                loop (next iteration) (s, c) isDone next

    let signalFirstLoop = loop 0 (0L, 0) (fun i _ -> i > (Seq.length amps)-1) (fun i -> i+1)
    loop 0 (signalFirstLoop, 0) (fun _ c -> c = 99) (fun i -> if i = (Seq.length amps)-1 then 0 else i+1)

let part2 acs =
    [5L..9L] |> permutations 5
    |> List.map (fun phases -> highestSignal phases acs)
    |> List.sort
    |> List.rev
    |> List.head

let test () =
    "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
    |> splitLine |> toInts
    |> part2
    |> printfn "Expected signal: 139629729, Actual: %d"

let p2 () =
    readText "input"
    |> splitLine |> toInts
    |> part2
    |> printfn "Expected signal: 15432220, Actual: %d"
