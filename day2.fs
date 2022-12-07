namespace Aoc

type Options =
    | Rock
    | Paper
    | Scissors

    member self.get_win_condition =
        match self with
        | Options.Rock -> Options.Paper
        | Options.Paper -> Options.Scissors
        | Options.Scissors -> Options.Rock

    member self.get_choice_score =
        match self with
        | Options.Rock -> 1
        | Options.Paper -> 2
        | Options.Scissors -> 3

module Day2 =
    open Utils.Helpers

    let file = load_file "inputs/day2.txt"

    let parse_file p1 =
        file
        |> Array.map (fun line ->
            let chars = line |> Seq.toArray

            let first, second =
                Array.splitAt (chars |> Array.findIndex (fun x -> x = ' ')) chars

            let remove_first arr = arr |> Array.removeAt 0

            let enemy_choice =
                if first |> System.String = "A" then Options.Rock
                else if first |> System.String = "B" then Options.Paper
                else Options.Scissors

            let my_choice =
                if p1 then
                    if second |> remove_first |> System.String = "X" then
                        Options.Rock
                    else if second |> remove_first |> System.String = "Y" then
                        Options.Paper
                    else
                        Options.Scissors
                else if second |> remove_first |> System.String = "X" then
                    enemy_choice.get_win_condition.get_win_condition
                else if second |> remove_first |> System.String = "Y" then
                    enemy_choice
                else
                    enemy_choice.get_win_condition

            (enemy_choice, my_choice))

    let calc_score p1 =
        parse_file p1
        |> Array.map (fun (enemy, me) ->
            if me = enemy.get_win_condition then
                6 + me.get_choice_score
            else if enemy = me.get_win_condition then
                me.get_choice_score
            else
                3 + me.get_choice_score)
        |> Array.sum

    let p1 = calc_score true
    let p2 = calc_score false

    let print_res = printfn "p1: %d\np2: %d" p1 p2
