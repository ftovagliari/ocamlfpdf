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

let (//) = Filename.concat
let main () = begin
  let filename = Sys.argv.(0) ^ ".pdf" in
  let outchan = open_out_bin filename in
  let close_file () = close_out outchan in
  begin
    try
      let doc = FPDF.create ~outchan () in
      FPDF.add_page doc;
      let x = 0. in
      let y = 0. in
      let height_image = 50. in
      let dirname = Filename.dirname Sys.executable_name in

      let size = 200. in
      FPDF.set_font ~family:`Times ~size doc;
      let scale = FPDF.scale doc in
      let width = size /. scale in
      let height = size /. scale in
      FPDF.text ~x ~y:height ~text:"AB" doc;

      let name = "Lena.png" in
      let data = Buffer.contents (FPDFUtil.fread (dirname // name)) in
      FPDF.image ~x ~y ~name ~data ~height:height_image doc;
      let name = "Lena.jpg" in
      let data = Buffer.contents (FPDFUtil.fread (dirname // name)) in
      FPDF.image ~x:(x +. height_image) ~y ~name ~data ~height:height_image doc;

      FPDF.close_document doc;
      close_file();
    with ex -> begin
      close_file();
      raise ex
    end
  end;
  Printf.printf "
  +-----------------------------------------------------------------------------
  |
  | Please see output file %s
  |
  +-----------------------------------------------------------------------------

%!" filename;
  if Sys.os_type = "Win32" then ignore (Sys.command filename)
  else ignore (kprintf Sys.command "xpdf %s" filename)
end

let _ = main ()
