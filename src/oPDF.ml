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

open PDFUtil
open Font
open Image
open Printf

(** @Deprecated Use module [PDF].  *)
class document ?orientation ?m_unit ?format () =
  let filename, outchan = Filename.open_temp_file "OCaml-FPDF" ".pdf" in
  let doc = PDF.create ?orientation ?m_unit ?format ~outchan () in
object (self)

  method scale = PDF.scale doc

  method set_margins ~left ?right ~top ?bottom () =
    PDF.set_margins ~left ~top ?right ?bottom doc

  method set_auto_page_break ?margin auto =
    PDF.set_auto_page_break ?margin auto doc

  method set_display_mode ?layout zoom =
    PDF.set_display_mode ?layout zoom doc

  method set_compression x =
    PDF.set_compression x doc

  method print_document ~filename =
    PDF.print_document doc

  method set_font ?family ?style ?size () =
    PDF.set_font ?family ?style ?size doc

  method open_document () =
    PDF.open_document doc

  method add_page ?orientation () =
    PDF.add_page ?orientation doc

  method close_document ~filename =
    PDF.close_document doc

  method text ~x ~y ~text () =
    PDF.text ~x ~y ~text doc

  method add_link ~x ~y ~width ~height ~link () =
    PDF.add_link ~x ~y ~width ~height ~link doc

  method get_string_width s =
    PDF.get_string_width s doc

  method set_draw_color ~red ?green ?blue () =
    PDF.set_draw_color ~red ?green ?blue doc

  method set_fill_color ~red ?green ?blue () =
    PDF.set_fill_color ~red ?green ?blue doc

  method set_text_color ~red ?green ?blue () =
    PDF.set_text_color ~red ?green ?blue doc

  method set_line_width width =
    PDF.set_line_width width doc

  method line ~x1 ~y1 ~x2 ~y2 =
    PDF.line ~x1 ~y1 ~x2 ~y2 doc

  method newline ?height () =
    PDF.newline ?height doc

  method cell ~width ?height ?text ?border ?padding ?ln ?align ?fill ?link () =
    PDF.cell ~width ?height ?text ?border ?padding ?ln ?align ?fill ?link doc

  method multi_cell ~width ~height ~text ?border ?padding ?align ?fill () =
    PDF.multi_cell ~width ~line_height:height ~text ?border ?padding ?align ?fill doc

  method multi_cell_lines ~width ~height ~text ?border ?padding ?align ?fill () =
    PDF.multi_cell_lines ~width ~height ~text ?border ?padding ?align ?fill doc

  method image ~name ~data ~x ~y ~image_width ~image_height ?width ?height ?format ?link () =
    PDF.image ~name ~data ~x ~y ~image_width ~image_height ?width ?height ?format ?link doc

  method rect ?x ?y ~width ~height ?radius ?style () =
    PDF.rect ?x ?y ~width ~height ?radius ?style doc

  method write ~height ~text ?link () =
    PDF.write ~height ~text ?link doc

  method set ?x ?y () =
    PDF.set ?x ?y doc
  method x = PDF.x doc
  method y = PDF.y doc
  method page_width = PDF.page_width doc
  method page_height = PDF.page_height doc
  method set_header callback = PDF.set_header_func callback doc
  method set_footer callback = PDF.set_footer_func callback doc
  method margins = PDF.margins doc
  method set_double_sided x = PDF.set_double_sided x doc
  method double_sided = PDF.double_sided doc
  method font_style = PDF.font_style doc
  method font_size = PDF.font_size doc
  method set_author x = PDF.set_author x doc
  method set_title x = PDF.set_title x doc
  method title = PDF.title doc
end
























