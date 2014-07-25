open Printf

let packages = [
  "fpdf",
  "version=\"1.0.0\"\ndescription=\"Library for generating PDF documents.\"\nrequires=\"str,unix,xml-light,zip\"\narchive(byte)=\"font/font.cma,fpdf.cma\"\narchive(native)=\"font/font.cmxa,fpdf.cmxa\"\n\npackage \"markup-editor\" (\n  version=\"1.0.0\"\n  description=\"Lablgtk widget for editing PDFMarkup.\"\n  requires=\"lablgtk2,xml-light\"\n  archive(byte)=\"gtk_pdfmarkup_editor.cma\"\n  archive(native)=\"gtk_pdfmarkup_editor.cmxa\"\n)\n",
  ["font/font.cma"; "fpdf.cma"; "font/font.cmxa"; "fpdf.cmxa"; "gtk_pdfmarkup_editor.cma"; "gtk_pdfmarkup_editor.cmxa"];
]

let _ = 
  if Array.length Sys.argv < 2 then failwith "Invalid parameters";
  let cwd = Sys.getcwd() in
  Sys.chdir "src";
  printf "Current working directory: %s\n%!" (Sys.getcwd());
  List.iter begin fun (name, defs, cmas) ->
    let arcs = String.concat " " (List.map (fun x -> (Filename.chop_extension x) ^ (if Sys.win32 then ".lib" else ".a")) (List.filter (fun x -> Filename.check_suffix x ".cmxa") cmas)) in
    let cmis = "" in
    let mlis = "" in
    if Sys.file_exists "META" then failwith "Cannot write META file: file exists";
    let chan = open_out_bin "META" in
    try
      output_string chan defs;
      let cmd =
        if List.mem Sys.argv.(1) ["-install"; "-reinstall"] then 
          sprintf "ocamlfind install %s META %s %s %s %s" name (String.concat " " cmas) arcs cmis mlis
        else if List.mem Sys.argv.(1) ["-uninstall"] then 
          sprintf "ocamlfind remove %s" name
        else failwith "Invalid parameters"
      in
      printf "%s\n%!" cmd;
      let exit_code = Sys.command cmd in
      if exit_code <> 0 then eprintf "Error: command %s exited with code %d\n%!" cmd exit_code;
      if Sys.file_exists "META" then (close_out_noerr chan; Sys.remove "META")
    with ex -> begin
      if Sys.file_exists "META" then (close_out_noerr chan; Sys.remove "META")
    end
  end packages;
  Sys.chdir cwd;
