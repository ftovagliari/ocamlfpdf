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

open PDFTypes
open PDFDocument
open PDFUtil
open Image
open Printf

(** set_font *)
let set_font ?family ?(style=([] : Font.style list)) ?size ?scale doc =
  let family = match (family : Font.family option) with None -> doc.font_family | x -> x in
  (* Select a font; size given in points. *)
  doc.underline <- List.mem `Underline style;
  let size = match size with None -> doc.font_size_pt | Some x -> x in (*if size = 0. then font_size_pt else size in*)
  (* Test if font is already selected *)
  if doc.font_family <> family || doc.font_style <> style || doc.font_size_pt <> size then begin
    (* Test if font is used for the first time *)
    let fkey = Font.key_of_font style (match family with None -> assert false | Some f -> f) in
    if not (PDFDocument.font_exists fkey doc) then begin
      try
        (* Check if one of the standard fonts *)
        let name = Font.get_name fkey in
        let cw = Font.get_metric fkey in
        let i = (List.length doc.fonts) + 1 in
        doc.fonts <- (fkey, {
          font_index = i;
          font_type = Core;
          font_name = name;
          font_up = -100;
          font_ut = 50;
          font_cw = cw;
          font_n = 0
        }) :: doc.fonts
      with Not_found -> failwith ("Undefined font: \"" ^ (Font.string_of_key fkey) ^ "\".")
    end;
    (* Select it *)
    doc.font_family <- family;
    doc.font_style <- style;
    doc.font_size_pt <- size;
    doc.font_size <- size /. doc.k;
    doc.font_scale <- scale;
    let current_font = List.assoc fkey doc.fonts in
    doc.current_font <- Some current_font;
    if n_pages doc > 0 then begin
      let font_scale = match doc.font_scale with None -> " 100 Tz" | Some z -> sprintf " %d Tz" z in
      print_buffer doc "BT /F%d %.2f Tf%s ET\n"
        current_font.font_index doc.font_size_pt font_scale
    end
  end;;

let font_style doc = doc.font_style
let font_size doc = doc.font_size_pt
let font_scale doc = doc.font_scale
let font_family doc = doc.font_family

