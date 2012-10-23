(*

  OCaml-FPDF
  Copyright (C) 2011 Francesco Tovagliari

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
open PDF
open PDFTypes
open PDFDocument

type t = {
  doc              : PDFDocument.t;
  mutable root_obj : int;
  mutable fields   : field list;
  mutable length   : int;
} and field = {
  id               : int;
  mutable obj      : int;
  x                : float;
  y                : float;
  width            : float;
  height           : float;
  parent           : field option;
  font_family      : Font.family option;
  font_size        : float;
  font_style       : Font.style list;
  name             : string;
  value            : string;
  default_value    : string;
  maxlength        : int option;
  readonly         : bool;
  numeric          : bool;
  border           : ([`Solid | `Underline | `Dashed] * string) option;
  bgcolor          : string option;
  fgcolor          : string;
  tooltip          : string option;
  page             : page;
}

(** find_font *)
let find_font ~fonts ~family ~style =
  let font_key = Font.key_of_font style (match family with Some x -> x | _ -> `Courier) in
  try List.assoc font_key fonts with Not_found -> assert false;;

(** instances *)
let instances = ref []

let js_validation_numeric = PDFUtil.escape "event.rc = event.change.toString().length <= 0 || event.change.match(\"[-.0-9]\")"

(** create *)
let create doc =
  let form = {
    doc      = doc;
    root_obj = 0;
    fields   = [];
    length   = 0;
  } in
  (** Add annot *)
  let _, doc_default_font = match List.rev doc.fonts with f :: _ -> f | _ -> assert false in
  PDFDocument.add_annot doc begin fun () ->
    if form.length > 0 then begin
      (* Appearance object *)
      PDFDocument.new_obj doc;
      let appearance_obj = PDFDocument.current_object_number doc in
      PDFDocument.print doc "<<";
      PDFDocument.print doc "/Type /XObject ";
      PDFDocument.print doc "/Subtype /Form ";
      (*PDFDocument.print doc "/BBox [ 0 0 100 12.5 ] ";*)
      (*PDFDocument.print doc "/Resources << /ProcSet [ /PDF /Text ] /Font << /F1 8 0 R >> >> "
        (*doc_default_font.font_index doc_default_font.font_n*);*)
      let stream = "" in
      PDFDocument.print doc "\n/Length %d\n" (String.length stream);
      PDFDocument.print_stream stream doc;
      PDFDocument.print doc ">>endobj\n";
      (* Fields *)
      form.fields <- List.rev form.fields;
      let page_height = PDF.page_height doc *. PDF.scale doc in
      let fields = List.map begin fun field ->
        let fg_color = sprintf "%s" (PDFUtil.rg_of_hex field.fgcolor) in
        let font = find_font ~fonts:doc.fonts ~family:field.font_family ~style:field.font_style in
        PDFDocument.new_obj doc;
        field.obj <- PDFDocument.current_object_number doc;
        PDFDocument.print doc "<<";
        PDFDocument.print doc "/Type /Annot /Subtype /Widget ";
        (* Field Area *)
        let x = field.x in
        let y = page_height -.field.y in
        PDFDocument.print doc "/Rect [%.2f %.2f %.2f %.2f] " x y (x +. field.width) (y -. field.height);
        PDFDocument.print doc "/FT /Tx ";
        PDFDocument.print doc "/Ff %d " (if field.readonly then 1 else 0);
        PDFDocument.print doc "/T(%s) " (PDFUtil.escape field.name);
        PDFDocument.print doc "/DV(%s) " (PDFUtil.escape field.default_value);
        PDFDocument.print doc "/V(%s) " (PDFUtil.escape field.value);
        PDFDocument.print doc "/DA (50 Tz /F%d %.2f Tf %s rg) " font.font_index field.font_size fg_color;
        (*PDFDocument.print doc "/AP <</N %d 0 R>> " appearance_obj;*)
        (match field.parent with Some parent -> PDFDocument.print doc "/Parent %d 0 R " parent.id | _ -> ());
        (match field.maxlength with Some maxlen -> PDFDocument.print doc "/MaxLen %d " maxlen | _ -> ());
        (if field.numeric then PDFDocument.print doc "/AA<</K<</S/JavaScript/JS(AFNumber_Keystroke\\(0,1,0,0,\"\",false\\))>>>> ");
        (*(if field.numeric then PDFDocument.print doc "/AA<</K<</S/JavaScript/JS(%s)>>>> " js_validation_numeric);*)
        (* Border *)
        let w, s, c =
          match field.border with
            | Some (`Solid, color) -> 1, "/S/S", Some color
            | Some (`Underline, color) -> 1, "/S/U", Some color
            | Some (`Dashed, color) -> 1, "/S/D", Some color
            | _ -> 0, "/S/S", None
        in
        let border_color = match c with
          | Some color -> sprintf "/BC[%s]" (PDFUtil.rg_of_hex color)
          | _ -> ""
        in
        let bg_color = match field.bgcolor with
          | Some color -> sprintf "/BG[%s]" (PDFUtil.rg_of_hex color)
          | _ -> ""
        in
        PDFDocument.print doc "/BS<</W %d%s>>/MK<<%s%s%s>> " w s
          (match field.tooltip with Some x -> sprintf "/CA%s " (PDFUtil.pdf_string x) | _ -> "")
          border_color bg_color;
        PDFDocument.print doc ">>endobj\n";
        field.obj
      end form.fields in
      fields
    end else []
  end;
  (** Add resource *)
  PDFDocument.add_resource begin fun () ->
    if form.length > 0 then begin
      (* Interactive Form Dictionary *)
      PDFDocument.new_obj doc;
      PDFDocument.print doc "<</Fields [ %s ] /NeedAppearances false "
        (String.concat " " (List.map (fun f -> sprintf "%d 0 R" f.obj) form.fields));
      PDFDocument.print doc "/DR <</Font <</F%d %d 0 R>>>> " (* Default resource dictionary *)
        doc_default_font.font_index doc_default_font.font_n;
      PDFDocument.print doc "/DA ()"; (* Default appearance string *)
      PDFDocument.print doc ">>endobj\n";
      form.root_obj <- PDFDocument.current_object_number doc;
    end
  end doc;
  (** Add catalog *)
  PDFDocument.add_catalog begin fun () ->
    if form.length > 0 then begin
      PDFDocument.print doc "/AcroForm %d 0 R " form.root_obj;
      (*PDFDocument.print doc "/OpenAction " form.root_obj;*)
    end;
    instances := List.filter (fun (k, _) -> doc != k) !instances
  end doc;
  form;;

(** add_text_field *)
let add_text_field ~x ~y ~width ~name
    ?height ?border ?bgcolor ?fgcolor
    ?font_family ?font_size ?font_style
    ?maxlength ?(readonly=false) ?(numeric=false)
    ?tooltip ?(value="") ?(default_value="")
    ?parent doc =
  let font_family = match font_family with Some x -> x | _ -> PDF.font_family doc in
  let font_size = match font_size with Some x -> x | _ -> PDF.font_size doc in
  let font_style = match font_style with Some x -> x | _ -> PDF.font_style doc in
  let fgcolor = match fgcolor with Some c -> c | _ -> PDFUtil.hex_of_rgb (PDF.text_color doc) in
  let height = match height with Some h -> h | _ -> font_size /. PDF.scale doc +. 1.5 in
  let form =
    try List.assq doc !instances
    with Not_found ->
      let x = create doc in
      instances := (doc, x) :: !instances;
      x
  in
  let id = form.length + 1 in
  let page = PDFDocument.get_current_page doc in
  let scale = PDF.scale doc in
  let field = {
    x      = x *. scale;
    y      = y *. scale;
    width  = width *. scale;
    height = height *. scale;
    obj    = 0;
    border; bgcolor; fgcolor; id; parent; font_family; font_size; font_style;
    name; value; default_value; maxlength; readonly; numeric; tooltip; page;
  } in
  form.fields <- field :: form.fields;
  form.length <- form.length + 1;
  field;;

