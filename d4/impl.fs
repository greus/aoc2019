module Day4

let input = [168630..718098]

let matchesRules number =
    let increaseOrSame (n1, n2) = n1 <= n2
    let adjacentSame (n1, n2) = n1 = n2

    let pairs = number |> string |> List.ofSeq |> Seq.pairwise |> Seq.toList

    if not <| List.forall increaseOrSame pairs then false
    elif not <| List.exists adjacentSame pairs then false
    else true

let implPart1 numbers = numbers |> List.filter matchesRules

let t1 () =
    [123788;111111;223450;123789]
    |> implPart1 // = 123788; 111111

let p1 () =
    input
    |> implPart1
    |> List.length

let twoAdjecentSame number =
    number |> string |> List.ofSeq
    |> Seq.groupBy (fun n -> n)
    |> Seq.exists (fun (k, v) -> v |> Seq.length = 2)

let implPart2 numbers =
    numbers
    |> implPart1
    |> List.filter twoAdjecentSame

let t2 () =
    [123444;111122]
    |> implPart2 // 111122

let p2 () =
    input
    |> implPart2
    |> List.length
