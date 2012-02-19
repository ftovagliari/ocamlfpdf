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

(** include_javascript *)
let include_javascript javascript doc =
  let js_object_number = ref 0 in
  if String.length javascript > 0 then begin
    PDFDocument.add_resource begin fun () ->
      PDFDocument.new_obj doc;
      js_object_number := PDFDocument.current_object_number doc;
      PDFDocument.print doc "<<\n";
      PDFDocument.print doc "/Names [(EmbeddedJS) %d 0 R ]\n" (PDFDocument.current_object_number doc + 1);
      PDFDocument.print doc ">>\n";
      PDFDocument.print doc "endobj\n";
      PDFDocument.new_obj doc;
      PDFDocument.print doc "<<\n";
      PDFDocument.print doc "/S /JavaScript\n";
      PDFDocument.print doc "/JS %s\n" (PDFUtil.pdf_string javascript);
      PDFDocument.print doc ">>\n";
      PDFDocument.print doc "endobj\n";
    end doc;
    PDFDocument.add_catalog begin fun () ->
      PDFDocument.print doc "/Names <</JavaScript %d 0 R>>\n" !js_object_number
    end doc
  end

(** set_autoprint *)
let set_autoprint ?printer_name ~dialog doc =
  let script = match printer_name with
    | None -> sprintf "print(%b);" dialog
    | Some pn -> String.concat "" [
      "var pp = getPrintParams();";
      begin
        if dialog then "pp.interactive = pp.constants.interactionLevel.full;"
        else "pp.interactive = pp.constants.interactionLevel.automatic;"
      end;
      (sprintf "pp.printerName = '%s';" pn);
      "print(pp);";
    ]
  in
  include_javascript script doc

