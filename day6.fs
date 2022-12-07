namespace Aoc

module Day6 =
    open Utils.Helpers

    let file = (load_file "inputs/day6.txt")[0]

    let parse_file pkt_amout =
        let file_chars = file |> Seq.toArray

        let rec get_packet acc pkt_amout =
            let tmp = acc |> Array.take pkt_amout

            if
                tmp
                |> Array.mapi (fun index char -> tmp |> Array.removeAt (index) |> Array.contains (char))
                |> Array.contains true
            then
                get_packet (acc |> Array.removeAt 0) pkt_amout
            else
                tmp

        get_packet file_chars pkt_amout

    let res1 = file.IndexOf((parse_file 4) |> System.String) + 4
    let res2 = file.IndexOf((parse_file 14) |> System.String) + 14

    let print_res = printfn "p1: %A\np2: %A" res1 res2
