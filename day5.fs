namespace Aoc

module Day5 =
    open Utils.Helpers

    let file = load_file "inputs/day5.txt"

    let parse_file =
        let crates, instructions =
            file |> Array.splitAt (file |> Array.findIndex (fun line -> (is_empty line)))

        let baseline = crates |> Array.find (fun x -> x.Contains '1')

        let indexed_base =
            baseline
            |> Seq.toArray
            |> Array.mapi (fun index char ->
                if (char = ' ') then
                    None
                else
                    Some(index, (char |> string |> int)))
            |> filter_none

        let crates_matrix =
            crates
            |> Array.removeAt (crates.Length - 1)
            |> Array.map (fun crate -> crate |> Seq.toArray)

        let mapped_crates =
            indexed_base
            |> Array.map (fun (base_index, base_val) ->
                (base_val,
                 crates_matrix
                 |> Array.map (fun crate_line ->
                     if crate_line[base_index] <> ' ' then
                         Some(crate_line[base_index])
                     else
                         None)
                 |> filter_none))
            |> Map

        let parsed_instructions =
            instructions
            |> Array.removeAt 0
            |> Array.map (fun line ->
                line.Split [| ' ' |]
                |> Array.removeAt 0
                |> Array.removeAt 1
                |> Array.removeAt 2
                |> Array.map (fun x -> x |> int))

        (mapped_crates, parsed_instructions)

    let res p2 =
        let mapped_crates, instructions = parse_file

        let rec reduce (map: Map<int, char[]>) index =
            if index >= instructions.Length then
                map
            else
                let remover = map.TryFind(instructions[index][1]).Value
                let adder = map.TryFind(instructions[index][2]).Value

                let removed = remover |> Array.take (instructions[index][0])
                let new_remover = remover |> Array.removeManyAt 0 removed.Length
                let added = adder |> Array.append (if p2 then removed else removed |> Array.rev)

                let res =
                    map
                    |> Map.remove (instructions[index][1])
                    |> Map.remove (instructions[index][2])
                    |> Map.add (instructions[index][1]) new_remover
                    |> Map.add (instructions[index][2]) added

                reduce (res) (index + 1)

        reduce mapped_crates 0
        |> Map.toArray
        |> Array.map (fun (_, x) -> x[0])
        |> System.String

    let print_res = printfn "p1: %s\np2: %s" (res false) (res true)
