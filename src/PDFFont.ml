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
open PDFUtil
open Printf
open Font

(** set_font *)
let set_font ?family ?(style=[]) ?size ?scale ?char_space doc =
  let family = match family with None -> doc.font_family | x -> x in
  (* Select a font; size given in points. *)
  doc.underline <- List.mem `Underline style;
  let size = match size with None -> doc.font_size_pt | Some x -> x in (*if size = 0. then font_size_pt else size in*)
  (* Test if font is already selected *)
  if doc.font_family <> family || doc.font_style <> style || doc.font_size_pt <> size then begin
    (* Test if font is used for the first time *)
    let fkey = Font.key_of_font style (match family with None -> assert false | Some f -> f) in
    if not (PDFDocument.font_exists fkey doc) then begin
      try
        let font_metrics = Font.find fkey in
        (* Check if one of the standard fonts *)
        let i = (List.length doc.fonts) + 1 in
        doc.fonts <- (fkey, {
          font_index = i;
          font_n = 0;
          font_metrics;
        }) :: doc.fonts
      with Not_found -> failwith ("Undefined font.")
    end;
    (* Select it *)
    doc.font_family <- family;
    doc.font_style <- style;
    doc.font_size_pt <- size;
    doc.font_size <- size /. doc.k;
    doc.font_scale <- scale;
    doc.font_char_space <- char_space;
    let current_font = List.assoc fkey doc.fonts in
    doc.current_font <- Some current_font;
    if n_pages doc > 0 then begin
      let font_scale = match doc.font_scale with None -> " 100 Tz" | Some z -> sprintf " %d Tz" z in
      let font_char_space = match doc.font_char_space with None -> " 0 Tc" | Some z -> sprintf " %f Tc" z in
      print_buffer doc "BT /F%d %f Tf%s%s ET\n"
        current_font.font_index doc.font_size_pt font_scale font_char_space
    end
  end;;

(** embed_font *)
let embed_font ~family ~style doc =
  let key = Font.key_of_font style family in
  if not (List.mem_assoc key doc.fonts) then begin
    try
      let font_metrics = Font.find key in
      begin
        match font_metrics.diff with
          | Some diff -> failwith "\"diff\" not implemented"
          | _ -> ()
      end;
      begin
        match font_metrics.fontFile with
          | Some _ when font_metrics.fontType = `TrueType ->
            doc.font_embed <- {
              fe_obj     = 0;
              fe_metrics = font_metrics;
            } :: doc.font_embed
          | _ -> ()
      end;
      doc.fonts <- (key, {
        font_index = List.length doc.fonts + 1;
        font_n = 0;
        font_metrics;
      }) :: doc.fonts
    with Not_found -> failwith "Font not found"
  end
;;

(** find_font_index *)
let find_font_index ?family ?(style=[]) doc =
  let family = match (family : Font.family option) with None -> doc.font_family | x -> x in
  let fkey = Font.key_of_font style (match family with None -> assert false | Some f -> f) in
  if not (PDFDocument.font_exists fkey doc) then begin
    try
      let font_metrics = Font.find fkey in
      (* Check if one of the standard fonts *)
      let name = font_metrics.fontName in
      let cw = font_metrics.charMetrics in
      let i = (List.length doc.fonts) + 1 in
      doc.fonts <- (fkey, {
          font_index = i;
          font_n = 0;
          font_metrics;
      }) :: doc.fonts
    with Not_found -> failwith ("Undefined font.")
  end;
  let font = List.assoc fkey doc.fonts in
  font.font_index
;;

let font_style doc = doc.font_style
let font_size doc = doc.font_size_pt
let font_scale doc = doc.font_scale
let font_char_space doc = doc.font_char_space
let font_family doc = doc.font_family

