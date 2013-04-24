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
open PDFDocument

type t = {
  doc                       : PDFDocument.t;
  mutable root_obj          : int;
  mutable fields            : field list;
  mutable calculation_order : field list option;
  mutable length            : int;
} and field = {
  id                        : int;
  mutable obj               : int;
  x                         : float;
  y                         : float;
  width                     : float;
  height                    : float;
  parent                    : field option;
  font_family               : Font.family option;
  font_size                 : float;
  font_style                : Font.style list;
  name                      : string;
  alt_name                  : string option;
  value                     : string;
  value_ap                  : string;
  default_value             : string;
  maxlength                 : int option;
  comb                      : int option;
  readonly                  : bool;
  hidden                    : bool;
  actions                   : action list;
  justification             : [`Left | `Center | `Right];
  border                    : ([`Solid | `Underline | `Dashed] * string) option;
  bgcolor                   : string option;
  fgcolor                   : string;
  bgcolor_ap                : string option;
  fgcolor_ap                : string;
  page                      : page;
} and action = [
  | `Keystroke of string
  | `Value_changed  of string
  | `Calculate of string
]

let (!!) = List.fold_left (fun acc (v, c) -> acc lor (if c then v else 0)) 0;;

(** instances *)
let instances = ref []

(** js_validation_numeric *)
let js_validation_numeric = PDFUtil.escape "event.rc = event.change.toString().length <= 0 || event.change.match(\"[-.0-9]\")"

(** get_calculation_order_default *)
let get_calculation_order_default form =
  List.rev (List.filter begin fun field ->
    List.exists (function `Calculate _ -> true | _ -> false) field.actions
  end form.fields);;

(** print_actions *)
let print_actions actions doc =
  if actions <> [] then begin
    PDFDocument.print doc "/AA<<";
    List.iter begin fun action ->
      let js =
        match action with
        | `Keystroke js -> PDFDocument.print doc "/K"; js
        | `Value_changed js -> PDFDocument.print doc "/V"; js
        | `Calculate js -> PDFDocument.print doc "/C"; js
      in
      PDFDocument.print doc "<</S/JavaScript/JS(%s)>>" (PDFUtil.escape js);
    end actions;
    PDFDocument.print doc ">>";
  end;;

(** create *)
let create doc =
  let form          = {
    doc               = doc;
    root_obj          = 0;
    fields            = [];
    calculation_order = None;
    length            = 0;
  } in
  let _, doc_default_font = match List.rev doc.fonts with f :: _ -> f | _ -> assert false in
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
      (* Calculation Order *)
      let co =
        match form.calculation_order with
          | Some co -> co
          | _ -> get_calculation_order_default form
      in
      let co = List.map (fun field -> sprintf "%d 0 R" field.obj) co in
      if co <> [] then (PDFDocument.print doc "/CO[%s]" (String.concat " " co));
      (*  *)
      PDFDocument.print doc ">>endobj\n";
      form.root_obj <- PDFDocument.current_object_number doc;
    end
  end doc;
  (** Add catalog *)
  PDFDocument.add_catalog begin fun () ->
    if form.length > 0 then begin
      PDFDocument.print doc "/AcroForm %d 0 R " form.root_obj;
    end;
    instances := List.filter (fun (k, _) -> doc != k) !instances
  end doc;
  form;;

(** get *)
let get doc =
  try List.assq doc !instances
  with Not_found ->
    let x = create doc in
    instances := (doc, x) :: !instances;
    x;;

(** fields *)
let fields form = form.fields

(** field_name *)
let field_name field = field.name

(** set_calculation_order *)
let set_calculation_order fields form = form.calculation_order <- Some fields

(** add_text_field *)
let add_text_field ~x ~y ~width ~height ~name ?alt_name
    ?border
    ?bgcolor ?fgcolor
    ?bgcolor_ap ?fgcolor_ap
    ?font_family ?(font_size(*=0.0*)) ?font_style
    ?maxlength ?comb ?(readonly=false) ?(hidden=false)
    ?(justification=`Left)
    ?(value="")
    ?value_ap
    ?(default_value="")
    ?(actions=[])
    ?parent form =
  let doc = form.doc in
  let font_family = match font_family with Some x -> x | _ -> PDF.font_family doc in
  let font_size = match font_size with Some x -> x | _ -> PDF.font_size doc in
  let font_style = match font_style with Some x -> x | _ -> PDF.font_style doc in
  let fgcolor = match fgcolor with Some c -> c | _ -> PDFUtil.hex_of_rgb (PDF.text_color doc) in
  let bgcolor_ap = match bgcolor_ap with None -> bgcolor | x -> x in
  let fgcolor_ap = match fgcolor_ap with None -> fgcolor | Some x -> x in
  let maxlength = match comb with None -> maxlength | comb -> comb in
  let value_ap = match value_ap with None -> value | Some x -> x in
  let id = form.length + 1 in
  let page = PDFDocument.get_current_page doc in
  let scale = PDF.scale doc in
  let field = {
    x      = x *. scale;
    y      = y *. scale;
    width  = width *. scale;
    height = height *. scale;
    obj    = 0;
    border; bgcolor; fgcolor; bgcolor_ap; fgcolor_ap; id; parent; font_family; font_size; font_style;
    name; alt_name; value; value_ap; default_value; maxlength; comb; readonly; hidden; actions;
    justification; page;
  } in
  form.fields <- field :: form.fields;
  form.length <- form.length + 1;
  let font =
    let family = field.font_family in
    let style = field.font_style in
    let f () = Font.find ?family ~style doc.fonts in
    match f () with Some x -> x | _ ->
      let old_family = PDF.font_family doc in
      let old_style = PDF.font_style doc in
      PDF.set_font ?family ~style doc;
      let x = f () in
      PDF.set_font ?family:old_family ~style:old_style doc;
      (match x with Some a -> a | _ -> assert false)
  in
  (** Add Field + Annotation dictionary *)
  PDFDocument.add_annot page begin fun page_obj ->
    let page_height = PDF.page_height doc *. PDF.scale doc in
    let x = field.x in
    let y = page_height -.field.y in
    (*let font = Font.find ?family:field.font_family ~style:field.font_style doc.fonts in*)
    let width = field.width in
    let height = field.height in
    let fg_color_ap = sprintf "%s" (PDFUtil.rg_of_hex field.fgcolor_ap) in
    let bg_color_ap = match field.bgcolor_ap with
      | Some color -> sprintf "q %s rg 0 0 %f %f re f Q " (PDFUtil.rg_of_hex color) width height
      | _ -> ""
    in
    let font_size = field.font_size in
    let padding = font_size /. 300. in
    let x_offset =
      match field.justification with
        | `Left -> padding
        | `Center -> (width -. PDFText.get_string_width_gen field.value_ap font font_size) /. 2.
        | `Right -> width -. PDFText.get_string_width_gen field.value_ap font font_size -. padding
    in
    (** Appearance object *)
    let stream = sprintf "%s/Tx BMC q BT /F%d %f Tf %s %f %f Td %s Tj ET Q EMC"
      bg_color_ap
      font.font_index font_size
      (sprintf "%s rg" fg_color_ap)
      x_offset ((height -. font_size) /. 2. +. font_size /. 8.5) (* Text position *)
      (PDFUtil.pdf_string field.value_ap)
    in
    PDFDocument.new_obj doc;
    let appearance_obj = PDFDocument.current_object_number doc in
    PDFDocument.print doc "<<";
    PDFDocument.print doc "/Type/XObject";
    PDFDocument.print doc "/Subtype/Form";
    PDFDocument.print doc "/BBox[0 0 %f %f]" width height;
    PDFDocument.print doc "/Length %d" (String.length stream);
    PDFDocument.print doc "/Resources<</ProcSet[/PDF /Text /ImageB /ImageC /ImageI]";
    PDFDocument.print doc "/Font<</F%d %d 0 R>>" font.font_index font.font_n;
    PDFDocument.print doc ">>>>\n";
    PDFDocument.print_stream stream doc;
    PDFDocument.print doc ">>endobj\n";
    PDFDocument.new_obj doc;
    field.obj <- PDFDocument.current_object_number doc;
    form.fields <- List.rev form.fields;
    PDFDocument.print doc "<<";
    PDFDocument.print doc "/AP<</N %d 0 R>>" appearance_obj;
    PDFDocument.print doc "/Type/Annot/Subtype/Widget";
    PDFDocument.print doc "/Rect [%f %f %f %f]" x y (x +. field.width) (y -. field.height);
    PDFDocument.print doc "/FT/Tx";
    PDFDocument.print doc "/P %d 0 R" page_obj;
    (match field.alt_name with Some x -> PDFDocument.print doc "/TU(%s)" x | _ -> ());
    let flags = !! [
      0b00000000_00000000_00000000_00000010, field.hidden;
      0b00000000_00000000_00000000_01000000, field.readonly;
      0b00000000_00000000_00000000_00000100, true;
    ] in
    PDFDocument.print doc "/F %d " flags;
    let flags = !! [
      0b00000000_00000000_00000000_00000001, field.readonly;
      0b00000001_00000000_00000000_00000000, (field.comb <> None);
    ] in
    PDFDocument.print doc "/Ff %d" flags;
    let field_name_escaped = PDFUtil.escape field.name in
    PDFDocument.print doc "/T(%s)" field_name_escaped;
    PDFDocument.print doc "/TM(%s)" field_name_escaped;
    PDFDocument.print doc "/DV(%s)" (PDFUtil.escape field.default_value);
    PDFDocument.print doc "/V(%s)" (PDFUtil.escape field.value);
    PDFDocument.print doc "/Q %d" (match field.justification with `Left -> 0 | `Center -> 1 | `Right -> 2);
    let fg_color = sprintf "%s" (PDFUtil.rg_of_hex field.fgcolor) in
    PDFDocument.print doc "/DA(/F%d %f Tf %s rg)" font.font_index field.font_size fg_color;
    (match field.parent with Some parent -> PDFDocument.print doc "/Parent %d 0 R " parent.id | _ -> ());
    (match field.maxlength with Some maxlen -> PDFDocument.print doc "/MaxLen %d " maxlen | _ -> ());
    print_actions field.actions doc;
    (** Border *)
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
    PDFDocument.print doc "/BS<</W %d%s>>/MK<<%s%s>>" w s border_color bg_color;
    PDFDocument.print doc ">>endobj\n";
    field.obj
  end;
  field;;

