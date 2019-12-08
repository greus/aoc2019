module Day6

let readLines filePath = System.IO.File.ReadLines(filePath)

let getRoot (map:(string * string) list) =
    let root = map |> List.find (fun (o, _) -> o = "COM")
    let rest = map |> List.filter (fun x -> x <> root)
    (root, rest)

type Tree =
  | Branch of string * Tree list
  | Leaf of string

let rec buildTree object map =
    let orbitees = map |> List.filter (fun (o, _) -> o = object)

    if orbitees = [] then
        Leaf object
    else
        Branch (object, List.map (fun (_, orbitee) -> buildTree orbitee map) orbitees)

let rec countOrbits lvl (acc: int list) tree =
    match tree with
    | Leaf node ->
        // printfn "%A %A" node lvl
        lvl::acc
    | Branch (node, children) ->
        // printfn "%A %A" node lvl
        let localAccum = lvl::acc
        let recurse = countOrbits (lvl + 1)
        let finalAccum = children |> Seq.fold recurse localAccum
        finalAccum

let impl map =
    let (root, orbitee), rest = getRoot map
    (root, orbitee)::rest
    |> buildTree root
    |> countOrbits 0 []
    |> List.sum

let t1 () =
    [("COM","B");("B","C");("C","D");("D","E");("E","F");("B","G");("G","H");("D","I");("E","J");("J","K");("K","L")]
    |> List.rev
    |> impl // = 42

let p1 () =
    let toTuple list = (List.head list, List.nth list 1)
    readLines "input"
    |> Seq.map (fun s -> s.Split ')' |> Seq.toList |> toTuple)
    |> Seq.toList
    |> impl
