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

let remove file = if Sys.file_exists file then Sys.remove file

let (//) = Filename.concat

let filter ext = List.filter (fun x -> Filename.check_suffix x ext)

let readdir dir = Array.to_list (Array.map (fun x -> dir // x) (Sys.readdir dir))

let rmr dir =
  let rmr = if Sys.os_type = "Win32" then "RMDIR /Q /S" else "rm -fr" in
  ignore (kprintf Sys.command "%s %s" rmr dir)

let _ =
  let tests = filter ".pdf" (readdir "tests") in
  List.iter remove tests;
  List.iter remove (filter ".html" (readdir (".." // "doc")));
  (*List.iter remove (filter ".css" (readdir (".." // "doc")));*)
  rmr (".." // ".tmp");
  rmr (".." // ".cache");
  rmr (".." // "bak");
;;
