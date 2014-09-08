(*

  This file is automatically generated by OCamlEditor 1.13.0, do not edit.

*)

open Printf

let packages = [
  (** fpdf *)
  "fpdf","\
version         = \"1.0.0\"
description     = \"Library for generating PDF documents.\"
requires        = \"str,unix,xml-light,zip\"
archive(byte)   = \"font.cma,fpdf.cma\"
archive(native) = \"font.cmxa,fpdf.cmxa\"

package \"markup-editor\" (
  version         = \"1.0.0\"
  description     = \"Lablgtk widget for editing PDFMarkup.\"
  requires        = \"lablgtk2,xml-light,fpdf\"
  archive(byte)   = \"gtk_pdfmarkup_editor.cma\"
  archive(native) = \"gtk_pdfmarkup_editor.cmxa\"
)",
  [
    "font/font.cma";
    "fpdf.cma";
    "font/font.cmxa";
    "fpdf.cmxa";
    "gtk_pdfmarkup_editor.cma";
    "gtk_pdfmarkup_editor.cmxa"
  ], [
    "fpdf_util.cmi";
    "fpdf_types.cmi";
    "gtk_pdfmarkup_editor.cmi";
    "fpdf_error.cmi";
    "fpdf_document.cmi";
    "fpdf_javascript.cmi";
    "fpdf.cmi";
    "fpdf_barcode.cmi";
    "fpdf_bookmark.cmi";
    "fpdf_pack.cmi";
    "fpdf_tabular.cmi";
    "fpdf_markup.cmi";
    "fpdf_form.cmi";
    "fpdf_table.cmi";
    "font/font.cmi"
  ], [
    "fpdf_util.mli";
    "fpdf_types.mli";
    "gtk_pdfmarkup_editor.mli";
    "fpdf_error.mli";
    "fpdf_document.mli";
    "fpdf_javascript.mli";
    "fpdf.mli";
    "fpdf_barcode.mli";
    "fpdf_bookmark.mli";
    "fpdf_pack.mli";
    "fpdf_tabular.mli";
    "fpdf_markup.mli";
    "fpdf_form.mli";
    "fpdf_table.mli";
    "font/font.mli"
  ];
]

let _ = 
  if Array.length Sys.argv < 2 then failwith "Invalid parameters";
  let cwd = Sys.getcwd() in
  Sys.chdir "src";
  let lib_ext = if Sys.win32 then ".lib" else ".a" in
  List.iter begin fun (name, defs, archives, (cmis : string list), (mlis : string list)) ->
    let cmxas = List.filter (fun x -> Filename.check_suffix x ".cmxa") archives in
    let libs = String.concat " " (List.map (fun x -> (Filename.chop_extension x) ^ lib_ext) cmxas) in
    let arcs = String.concat " " archives in
    let cmis = String.concat " " cmis in
    let mlis = String.concat " " mlis in
    let _META = "META" in
    if Sys.file_exists _META then failwith "Cannot write META file: file exists";
    let chan = open_out_bin _META in
    try
      let sudo = if Sys.win32 || List.mem Sys.argv.(1) ["print"] then "" else "sudo -E " in
      let cmd = ref [] in
      if not (List.mem Sys.argv.(1) ["install"; "uninstall"; "reinstall"; "print"]) then failwith "Invalid parameters";
      output_string chan defs;
      flush chan;
      if List.mem Sys.argv.(1) ["uninstall"; "reinstall"] then cmd := (sprintf "%socamlfind remove %s" sudo name) :: !cmd;
      if List.mem Sys.argv.(1) ["install"; "reinstall"; "print"] then 
        cmd := (sprintf "%socamlfind install %s %s %s %s %s %s" sudo name _META arcs libs cmis mlis) :: !cmd;
      assert (!cmd <> []);
      let cmd = String.concat " && " (List.rev !cmd) in
      if List.mem Sys.argv.(1) ["print"] then printf "\n##### %s.%s #####\n\n%s\n\n# %s\n%!" _META name defs cmd;
      let exit_code = if Sys.argv.(1) = "print" then 0 else Sys.command cmd in
      if exit_code <> 0 then eprintf "Error: command %s exited with code %d\n%!" cmd exit_code;
      if Sys.file_exists _META then (close_out_noerr chan; Sys.remove _META)
    with ex -> begin
      if Sys.file_exists _META then (close_out_noerr chan; Sys.remove _META)
    end
  end packages;
  Sys.chdir cwd;
