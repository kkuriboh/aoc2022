namespace Aoc

module Day4 =
    open Utils.Helpers
    let file = load_file "inputs/day4.txt"

    let parse_file =
        file
        |> Array.map (fun line -> line.Split [| ',' |])
        |> Array.map (fun line ->
            let first = line[ 0 ].Split [| '-' |]
            let second = line[ 1 ].Split [| '-' |]
            (set { first[0] |> uint32 .. first[1] |> uint32 }, set { second[0] |> uint32 .. second[1] |> uint32 }))

    let res1 =
        parse_file
        |> Array.filter (fun (left, right) ->
            let intersection = Set.intersect left right |> Set.toArray
            intersection = (left |> Set.toArray) || intersection = (right |> Set.toArray))
        |> Array.length

    let res2 =
        parse_file
        |> Array.filter (fun (left, right) ->
            let intersection = Set.intersect left right |> Set.toArray
            intersection.Length <> 0)
        |> Array.length

    let print_res = printfn "p1: %d\np2: %d" res1 res2
