namespace Aoc

module Day1 =
    open Utils.Helpers

    let calories =
        load_file "inputs/day1.txt"
        |> Array.map (fun line ->
            if System.String.IsNullOrEmpty line then
                None
            else
                Some(line |> int))

    let rec separate arr =
        fun data ->
            let index = arr |> Array.tryFindIndex (fun x -> x = None)

            if index.IsSome then
                let first, second = arr |> Array.splitAt index.Value
                separate (second |> Array.removeAt 0) (Array.append [| first |> Array.map (fun x -> x.Value) |] data)
            else if arr.Length <> 0 && index.IsNone then
                data |> Array.append [| arr |> Array.map (fun x -> x.Value) |]
            else
                data

    let data = separate calories Array.empty

    let sorted_calories =
        data |> Array.map (fun x -> x |> Array.sum) |> Array.sortDescending

    let print_res =
        printfn "p1: %d\np2: %d" sorted_calories[0] (sorted_calories[0] + sorted_calories[1] + sorted_calories[2])
