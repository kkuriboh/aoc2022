namespace Utils

module public Helpers =
    let load_file file_path = System.IO.File.ReadAllLines file_path

    let is_empty str = System.String.IsNullOrEmpty str

    let filter_none<'T> (arr: 'T option[]) =
        arr |> Array.filter (fun x -> x.IsSome) |> Array.map (fun y -> y.Value)
