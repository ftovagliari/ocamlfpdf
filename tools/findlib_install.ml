(*

  OCaml-FPDF
  Copyright (C) 2010-2013 Francesco Tovagliari

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version,
  with the special exception on linking described in file LICENSE.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*)


open Printf

(** pushd and popd *)
let pushd, popd =
  let stack = Stack.create () in
  begin fun dir ->
    let cwd = Sys.getcwd () in
    Stack.push cwd stack;
    Sys.chdir dir
  end, (fun () -> Sys.chdir (Stack.pop stack))

let libname = "fpdf"
let cmas = ["fpdf"; "gtk_pdfmarkup_editor"; "font/font"]

let _ =
  let ar = String.concat " " (List.map (fun x -> x ^ (if Sys.os_type = "Win32" then ".lib" else ".a")) cmas) in
  let find ?(dir=".") ext =
    let files = Array.to_list (Sys.readdir dir) in
    let files = List.filter (fun x -> Filename.check_suffix x ext) files in
    String.concat " " (List.map (Filename.concat dir) files)
  in
  let cmas = List.map (fun x -> [x ^ ".cma"; x ^ ".cmxa"]) cmas in
  let cmas = String.concat " " (List.flatten cmas) in
  pushd "src";
  ignore (kprintf Sys.command "ocamlfind install %s META %s %s %s %s %s %s"
            libname cmas ar (find ".cmi") (find ".mli")
            (find ~dir:"font" ".cmi") (find ~dir:"font" ".mli"));
  popd();
;;



