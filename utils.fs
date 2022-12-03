namespace Utils

module public Helpers =
    let load_file file_path = System.IO.File.ReadAllLines file_path
