(*

  OCaml-FPDF
  Copyright (C) 2010-2012 Francesco Tovagliari

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
open PDFTypes

let (//) = Filename.concat

let main () = begin
  let filename = Sys.argv.(0) ^ ".pdf" in
  let outchan = open_out_bin filename in
  let close_file () = close_out outchan in
  begin
    try
      let doc = PDF.create ~outchan () in
      PDF.set_display_mode (`Custom_zoom 300.) doc;
      PDF.set_open_actions [
        `ResetForm;
        `GoTo {dest_page = 0; dest_display = `FitV None}
      ] doc;

      let margin = 20. in
      PDF.set_margins ~left:margin ~top:margin doc;
      PDF.add_page doc;

      (** Form fields *)
      PDF.set_font ~family:`Helvetica ~size:20. doc;
      let x = margin in
      let y = margin in
      PDFGraphics.rect ~x ~y ~width:(PDF.page_width doc -. 2. *. margin) ~height:(PDF.page_height doc -. 2. *. margin) doc;
      let width = PDF.page_width doc -. margin *. 2. in
      let height = PDF.page_height doc -. margin *. 2. in
      let _ = PDFMarkup.print ~x ~y:(y +. 50.) ~width ~markup:"Test" doc in
      let field =
        PDFForm.add_text_field ~x ~y ~width:80.
          ~maxlength:3 ~readonly:false ~numeric:true
          ~name:"test_field" ~value:"" ~default_value:"3"
          ~bgcolor:"#f0f0f0" (*~border:(`Underline, "#000000")*) doc
      in

      (** Close PDF document *)
      PDF.close_document doc;
      close_file();
    with ex -> begin
      close_file();
      raise ex
    end
  end;
  if Sys.os_type = "Win32" then ignore (Sys.command filename);
end

let _ = main ()
