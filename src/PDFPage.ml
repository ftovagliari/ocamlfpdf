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

open PDFTypes
open PDFDocument
open PDFFont
open PDFUtil
open PDFImages
open Printf

let page_num doc = doc.page

let page_count = PDFDocument.n_pages

let get_page n doc =
  try
    let len = List.length doc.pages in
    List.nth doc.pages (len - n - 1)
  with Invalid_argument "List.nth" -> failwith ("get_page (" ^ (string_of_int n) ^ ")")

let page_width doc = doc.w
let page_height doc = doc.h

let set_header_func callback doc = doc.header <- callback
let set_footer_func callback doc = doc.footer <- callback

let set_margins ~left ?(right=left) ~top ?(bottom=top) doc =
  doc.l_margin <- left;
  doc.r_margin <- right;
  doc.t_margin <- top;
  doc.b_margin <- bottom

let margins doc = doc.t_margin, doc.r_margin, doc.b_margin, doc.l_margin


let add_page ?orientation doc =
  if doc.state = Begin_document then open_document doc;
  let family = doc.font_family in
  let style = if doc.underline then `Underline :: doc.font_style else doc.font_style in
(*  let style = (string_of_style font_style) ^ (if underline then "U" else "") in*)
  let size = doc.font_size_pt in
  let save () = doc.line_width, doc.drawColor, doc.fillColor, doc.textColor, doc.colorFlag in
  let restore (lw, dc, fc, tc, cf) =
    if doc.line_width <> lw then begin
      doc.line_width <- lw;
      print_buffer doc "%f w\n" (lw *. doc.k);
    end;
    may ~f:(fun family -> set_font ~family ~style ~size doc) family;
    if doc.drawColor <> dc then begin
      doc.drawColor <- dc;
      print_buffer doc "%s\n" dc
    end;
    if doc.fillColor <> fc then begin
      doc.fillColor <- fc;
      print_buffer doc "%s\n" fc
    end;
    doc.textColor <- tc;
    doc.colorFlag <- cf
  in
  let old_lw = doc.line_width in
  if n_pages doc > 0 then begin
    doc.inFooter <- true;
    call_and_restore ~pre:save doc.footer () ~post:restore;
    doc.inFooter <- false;
    end_page doc
  end;
  begin_page ?orientation doc;
  (* Set line cap style to square *)
  PDFGraphicsState.set_line_cap (PDFGraphicsState.line_cap doc) doc;
  (*print_buffer doc "2 J\n";*)
  call_and_restore ~pre:save doc.header () ~post:restore;
  PDFGraphicsState.set_line_width old_lw doc;;
