open Printf

let (|>) a b = if a = 0 then b else 2

let _ =
  let sudo = if Sys.win32 then "" else "sudo -E " in
  List.fold_left begin fun acc c ->
    if acc = 0 then (printf "%s\n%!" c; Sys.command c) else acc
  end 0 [
    "ocaml tools/distclean.ml";
    "ocaml build.ml markup-editor";
    sprintf "%socamlfind remove fpdf" sudo;
    sprintf "%socaml tools/findlib_install.ml" sudo;
  ]

