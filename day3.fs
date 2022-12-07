namespace Aoc

module Day3 =
    open Utils.Helpers

    let file = load_file "inputs/day3.txt"

    let get_char_priority char =
        if (char |> System.Char.IsLower) then
            (char |> uint8) - 0x60uy
        else
            (char |> uint8) - 0x26uy

    let get_res1 =
        file
        |> Array.map (fun line -> line |> Seq.toArray |> Array.splitAt (line.Length / 2))
        |> Array.map (fun (left, right) ->
            (left |> Array.filter (fun char -> right |> Array.contains char)).[0]
            |> get_char_priority
            |> uint32)
        |> Array.sum

    let get_res2 =
        file
        |> Array.splitInto (file.Length / 3)
        |> Array.map (fun group ->
            let group_chars = group |> Array.map (fun rucksack -> rucksack |> Seq.toArray)

            let first_filter =
                group_chars.[0] |> Array.filter (fun x -> group_chars.[1] |> Array.contains x)

            let second_filter =
                group_chars.[1] |> Array.filter (fun x -> group_chars.[2] |> Array.contains x)

            (first_filter |> Array.filter (fun x -> second_filter |> Array.contains x))[0])
        |> Array.map (fun x -> get_char_priority x |> uint32)
        |> Array.sum

    let print_res = printfn "p1: %d\np2: %d" get_res1 get_res2
