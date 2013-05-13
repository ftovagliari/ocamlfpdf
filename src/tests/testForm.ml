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
      let margin = 20. in
      PDF.set_margins ~left:margin ~top:margin doc;
      PDF.set_font ~family:`Helvetica ~size:40. doc;
      PDF.set_open_actions [
        (*`ResetForm;*)
        `GoTo {dest_page = 0; dest_display = `FitH None}
      ] doc;

      let form = PDFForm.get doc in

      let create_page default_value =
        PDF.add_page doc;
        let x = margin in
        let y = margin in
        PDFGraphics.rect ~x ~y ~width:(PDF.page_width doc -. 2. *. margin) ~height:(PDF.page_height doc -. 2. *. margin) doc;
        let name = sprintf "text_field_%d_1" (PDF.page_count doc) in
        let field_1 =
          PDFForm.add_text_field ~x ~y ~width:160. ~height:20.
            ~maxlength:5 ~readonly:false ~hidden:false ~justification:`Center
            ~name ~alt_name:name
            ~value:"" ~value_ap:"Enter a number here..."
            ~default_value:""
            ~bgcolor:"#f0f0f0" ~fgcolor_ap:"#c0c0c0"
            ~actions:[
              `Keystroke "AFNumber_Keystroke(0,1,0,0,\"\",false);";
            ]
            (*~border:(`Dashed, "#000000")*) form
        in
        ignore (PDFMarkup.print ~x:(x +. 80.) ~y ~width:50. ~markup:"3" doc);
        let y = y +. 30. in
        let name = sprintf "text_field_%d_2" (PDF.page_count doc) in
        let field_2 =
          PDFForm.add_text_field ~x ~y ~width:100. ~height:5. ~font_size:8.
            ~readonly:false ~hidden:false ~justification:`Left
            ~name ~alt_name:name
            ~value:default_value ~default_value
            ~actions:(
              (if PDF.page_count doc = 2 then
              [`Calculate "AFSimple_Calculate(\"SUM\", new Array (\"text_field_1_1\", \"text_field_1_2\"));"] else [])
              @ [`Keystroke "AFNumber_Keystroke(0,1,0,0,\"\",false);";]
            )
            ~bgcolor:"#fff0f0" (*~border:(`Dashed, "#000000")*) form
        in
        field_1, field_2
      in
      ignore (create_page "");
      let _, field_calc = create_page "" in
      ignore (create_page "88888");

      PDFForm.set_calculation_order [field_calc] form;

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
