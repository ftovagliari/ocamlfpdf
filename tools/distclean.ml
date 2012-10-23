let remove file = if Sys.file_exists file then Sys.remove file

let (//) = Filename.concat

let filter ext = List.filter (fun x -> Filename.check_suffix x ext)

let readdir dir = Array.to_list (Array.map (fun x -> dir // x) (Sys.readdir dir))


let _ =
  let tests = filter ".pdf" (readdir "tests") in
  List.iter remove tests;
  let doc = readdir (".." // "doc") in
  List.iter remove doc;
;;
