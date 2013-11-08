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

open Fpdf_types
open Printf
open Fpdf_util
open Fpdf_images
open Font

type state = Begin_document | End_page | Begin_page | End_document

type link = (float * float * float * float * link_dest) list
and link_dest = Uri of string | Internal of int

type obj = {mutable obj_offset : int}

and font_obj = {
  font_metrics       : Font.t;
  mutable font_n     : int;
  mutable font_index : int;
}
and font_embed = {
  fe_metrics         : Font.t;
  mutable fe_obj     : int;
}
and page = {
  mutable pg_buffer             : Buffer.t;
  mutable pg_change_orientation : bool;
  mutable pg_link               : link option;
  mutable pg_annots             : annot;
  mutable pg_obj                : int;
}
and annot = {
  mutable annot_obj  : int list;
  mutable annot_func : (int -> int) list
} and t = {
  mutable page                  : int;
  mutable current_object_number : int;
  mutable objects               : obj list;
  mutable pages                 : page list;                (* array containing pages *)
  mutable state                 : state;                    (* current document state *)
  mutable compress              : bool;                     (* compression flag *)
  mutable def_orientation       : orientation;              (* default orientation *)
  mutable cur_orientation       : orientation;              (* current orientation *)
  mutable k                     : float;                    (* scale factor (number of points in user unit) *)
  mutable fw_pt                 : float;
  mutable fh_pt                 : float;                    (* dimensions of page format in points *)
  mutable fw                    : float;
  mutable fh                    : float;                    (* dimensions of page format in user unit *)
  mutable w_pt                  : float;
  mutable h_pt                  : float;                    (* current dimensions of page in points *)
  mutable w                     : float;
  mutable h                     : float;                    (* current dimensions of page in user unit *)
  mutable l_margin              : float;                    (* left margin*)
  mutable t_margin              : float;                    (* top margin*)
  mutable r_margin              : float;                    (* right margin*)
  mutable b_margin              : float;                    (* page break margin *)
  mutable c_margin              : float;                    (* cell margin *)
  mutable pos_x                 : float;
  mutable pos_y                 : float;                    (* current position in user unit for cell positioning *)
  mutable lasth                 : float;                    (* height of last cell printed *)
  mutable line_width            : float; (*0.1 /. scale*)   (* line width in user unit (0.567 = 0.2 mm) *)
  mutable line_cap              : line_cap_style;           (*  *)
  mutable line_join             : line_join_style;          (*  *)
  mutable line_dash             : (int list * int);         (*  *)
  mutable fonts                 : (Font.key * font_obj) list;   (* array of used fonts [(key, font) list] *)
  mutable font_embed            : font_embed list;           (* array of font files *)
  mutable diffs                 : string list;              (* array of encoding differences *)
  mutable images                : Fpdf_images.Table.t;         (* array of used images *)
  mutable links                 : (int * float) list;       (* array of internal links *)
  mutable font_family           : Font.family option;       (* current font family *)
  mutable font_style            : Font.style list;          (* current font style *)
  mutable underline             : bool;                     (* underlining flag *)
  mutable current_font          : font_obj option;          (* current font info *)
  mutable font_size_pt          : float;                    (* current font size in points *)
  mutable font_size             : float;                    (* current font size in user unit *)
  mutable font_scale            : int option;               (* current font scale *)
  mutable font_char_space       : float option;             (* current char space *)
  mutable drawColor             : string;                   (* commands for drawing color *)
  mutable fillColor             : string;                   (* commands for filling color *)
  mutable textColor             : string;                   (* commands for text color *)
  mutable colorFlag             : bool;                     (* indicates whether fill and text colors are different *)
  mutable ws                    : float;                    (* word spacing *)
  mutable auto_page_break       : bool;                     (* automatic page breaking *)
  mutable pageBreakTrigger      : float;                    (* threshold used to trigger page breaks *)
  mutable inFooter              : bool;                     (* flag set when processing footer *)
  mutable zoomMode              : zoom;                     (* zoom display mode *)
  mutable layoutMode            : layout;                   (* layout display mode *)
  mutable title                 : string;                   (* title *)
  mutable subject               : string;                   (* subject *)
  mutable author                : string;                   (* author *)
  mutable keywords              : string list;              (* keywords *)
  mutable creator               : string;                   (* creator *)
  mutable creation_date         : string;                   (* creationDate *)
  mutable aliasNbPages          : string;                   (* alias for total number of pages *)
  mutable pdfVersionMajor       : int;                      (* PDF version number *)
  mutable pdfVersionMinor       : int;                      (* PDF version number *)
  mutable header                : (unit -> unit);
  mutable footer                : (unit -> unit);
  mutable double_sided          : bool;
  mutable current_length        : int;
  mutable outchan               : out_channel;
  mutable print_resources       : (unit -> unit) list;
  mutable print_catalog         : (unit -> unit) list;
  mutable text_color_rgb        : int * int * int;
  mutable fill_color_rgb        : int * int * int;
  mutable draw_color_rgb        : int * int * int;
  mutable open_action_obj       : int option;
  mutable open_actions          : action list;
}

let open_document doc = match doc.state with
  | End_document -> failwith "Fpdf.open_document: document already closed."
  | _ -> doc.state <- End_page

let n_pages doc = List.length doc.pages

let string_of_style style =
  let bold = List.mem `Bold style in
  let italic = List.mem `Italic style in
  Printf.sprintf "%S%S"
    (if bold then "B" else "") (if italic then "I" else "")

let find_font ?family ~style doc =
  let font_key = Font.key_of_font style (match family with Some x -> x | _ -> `Courier) in
  try Some (List.assoc font_key doc.fonts) with Not_found -> None;;

let get_current_page doc =
  let len = List.length doc.pages in
  try
    List.nth doc.pages (len - doc.page - 1)
  with Failure "nth" -> kprintf failwith "get_current_page (%d, %d)" len doc.page

let get_link index doc =
  try List.nth doc.links index
  with Invalid_argument "List.nth" -> failwith ("get_link (" ^ (string_of_int index) ^ ")")

let find_object n doc =
  try List.nth doc.objects ((List.length doc.objects) - n)
  with _ -> failwith ("find_object (" ^ (string_of_int n) ^ ")")

let get_buffer ~create doc =
  match doc.state with
    | Begin_page -> (get_current_page doc).pg_buffer
    | _ -> assert false

let font_exists key doc =
  try ignore(List.assoc key doc.fonts); true
  with Not_found -> false

let print_buffer doc =
  let current = (get_current_page doc).pg_buffer in
  Printf.ksprintf begin fun str ->
    Buffer.add_string current str
  end

let print doc =
  Printf.ksprintf begin fun str ->
    doc.current_length <- doc.current_length + (String.length str);
    output_string doc.outchan str;
  end

let print_header doc =
  print doc "%%PDF-%d.%d\n%%\161\179\197\215\n" doc.pdfVersionMajor doc.pdfVersionMinor

let print_stream str doc =
  print doc "stream\n";
  print doc "%s\n" str;
  print doc "endstream\n"

let print_trailer doc =
  print doc "/Size %d\n" (doc.current_object_number + 1);
  print doc "/Root %d 0 R\n" doc.current_object_number; (* object_index[Catalog] *)
  print doc "/Info %d 0 R\n" (doc.current_object_number - 1); (* object_index[Info] *)
  let id = sprintf "%f%s%s" (Unix.gettimeofday ()) doc.title doc.author in
  let id = Digest.string id in
  let id = Digest.to_hex id in
  print doc "/ID [<%s><%s>]\n" id id

let print_info doc =
  print doc "/Producer %s" (pdf_string "OCaml-FPDF");
  if doc.title <> "" then
    print doc "/Title %s" (pdf_string doc.title);
  if doc.subject <> "" then
    print doc "/Subject %s" (pdf_string doc.subject);
  if doc.author <> "" then
    print doc "/Author %s" (pdf_string doc.author);
  if List.length doc.keywords <> 0 then
    print doc "/Keywords %s" (pdf_string (String.concat " " doc.keywords));
  if doc.creator <> "" then
    print doc "/Creator %s" (pdf_string doc.creator);
  if doc.creation_date <> "" then
    print doc "/CreationDate %s" doc.creation_date;;

let print_catalog doc =
  print doc "/Type /Catalog ";
  print doc "/Pages 1 0 R ";
  (match doc.open_action_obj with Some x -> print doc "/OpenAction %d 0 R " x | _ -> ());
  begin match doc.layoutMode with
    | `Single -> print doc "/PageLayout /SinglePage "
    | `Continuous -> print doc "/PageLayout /OneColumn "
    | `Two -> print doc "/PageLayout /TwoColumnLeft "
    | _ -> () (*| Default_layout*)
  end;
  List.iter (fun f -> f()) (List.rev doc.print_catalog);
  doc.print_catalog <- []

(** Begin a new object *)
let new_obj doc =
   doc.current_object_number <- doc.current_object_number + 1;
   let obj = {obj_offset = doc.current_length} in
   doc.objects <- obj :: doc.objects;
   print doc "%d 0 obj" doc.current_object_number

(** Print pages *)
let print_pages doc =
  let nb = doc.page in
  if doc.aliasNbPages <> "" then
    (* Replace number of pages *)
    List.iter begin fun pg ->
      let pg_content =
        Str.global_replace (Str.regexp doc.aliasNbPages) (string_of_int (nb + 1)) (Buffer.contents pg.pg_buffer) in
      Buffer.clear pg.pg_buffer;
      Buffer.add_string pg.pg_buffer pg_content
    end doc.pages;
  let w_pt, h_pt = match doc.def_orientation with
    | `Portrait -> doc.fw_pt, doc.fh_pt
    | `Landscape -> doc.fh_pt, doc.fw_pt in
  let filter = if doc.compress then "/Filter /FlateDecode " else "" in
  List.iter begin fun page ->
    (* Page *)
    let page_obj_num = doc.current_object_number + 1 + 2 * (List.length page.pg_annots.annot_func) in
    page.pg_annots.annot_obj <- List.rev_map (fun f -> f page_obj_num) page.pg_annots.annot_func;
    new_obj doc;
    page.pg_obj <- doc.current_object_number;
    print doc "<</Type /Page ";
    print doc "/Parent 1 0 R ";
    if page.pg_change_orientation then print doc "/MediaBox [0 0 %f %f] " h_pt w_pt;
    print doc "/Resources 2 0 R ";
    if page.pg_annots.annot_obj <> [] then begin
      print doc "/Annots [ %s ] " (String.concat " " (List.map (sprintf "%d 0 R") page.pg_annots.annot_obj))
    end;
    print doc "/Contents %d 0 R>>" (doc.current_object_number + 1);
    print doc "endobj\n";
    (* Page content *)
    let p = Buffer.contents page.pg_buffer in
    page.pg_buffer <- Buffer.create 0;
    let p = if doc.compress then gz_compress p else p in
    new_obj doc;
    print doc "<<%s/Length %d>>\n" filter (String.length p);
    print_stream p doc;
    print doc "endobj\n";
  end (List.rev doc.pages);
  (* Pages root *)
  (find_object 1 doc).obj_offset <- doc.current_length;
  print doc "1 0 obj";
  print doc "<</Type /Pages ";
  print doc "/Kids [ %s ] " (String.concat " " (List.map (fun page -> sprintf "%d 0 R" page.pg_obj) (List.rev doc.pages)));
  print doc "/Count %d " (nb + 1);
  print doc "/MediaBox [0 0 %f %f] " w_pt h_pt;
  print doc ">>endobj\n"

let get_font_path () = failwith "get_font_path" (*"../fpdf153/font"*)

(** print_fonts *)
let print_fonts doc =
  (* Encodings *)
  List.iter begin fun diff ->
    new_obj doc;
    print doc "<</Type /Encoding /BaseEncoding /WindAnsiEncoding /Differences [%s]>>endobj\n" diff;
  end doc.diffs;
  (** Font file embedding *)
  List.iter begin fun fe  ->
    match fe.fe_metrics.fontFile with
      | None -> assert false
      | Some (original_size, fontFile) ->
        new_obj doc;
        fe.fe_obj <- doc.current_object_number;
        print doc "<</Length %d " (String.length fontFile);
        if true (*compressed*) then print doc "/Filter /FlateDecode ";
        print doc "/Length1 %d " original_size;
        print doc ">>\n";
        print_stream fontFile doc;
        print doc "endobj\n"
  end doc.font_embed;
  (** Font objects *)
  List.iter begin fun (_, fnt) ->
    fnt.font_n <- doc.current_object_number + 1;
    let typ, name = fnt.font_metrics.fontType, fnt.font_metrics.fontName in
    match typ with
      | `Core ->
        (* Standard font *)
        new_obj doc;
        print doc "<</Type /Font ";
        print doc "/BaseFont /%s " name;
        print doc "/Subtype /Type1 ";
        if name <> "Symbol" && name <> "ZapfDingbats" then print doc "/Encoding /WinAnsiEncoding ";
        print doc ">>endobj\n";
      | `Type1 | `TrueType  ->
        (* Additional Type1 or TrueType/OpenType font *)
        new_obj doc;
        let first_char = 32 in
        let last_char = 255 in
        print doc "<</Type /Font ";
        print doc "/BaseFont /%s " name;
        print doc "/Subtype /%s " (match typ with `Type1 -> "Type1" | _ -> "TrueType");
        print doc "/FirstChar %d /LastChar %d " first_char last_char;
        print doc "/Widths %d 0 R " (doc.current_object_number + 1);
        print doc "/FontDescriptor %d 0 R " (doc.current_object_number + 2);
        print doc "/Encoding /WinAnsiEncoding ";
        print doc ">>endobj\n";
        (* Widths *)
        new_obj doc;
        let widths = ref [] in
        let metrics = fnt.font_metrics in
        let cw = metrics.charMetrics in
        for i = last_char downto first_char do widths := string_of_int (cw (Char.chr i)) :: !widths done;
        print doc "\n[%s]\nendobj\n" (String.concat " " !widths);
        (* Descriptor *)
        new_obj doc;
        print doc "<</Type /FontDescriptor /FontName /%s " name;
        let a, b, c, d = metrics.fontBBox in
        print doc "/FontBBox [%d %d %d %d] " a b c d;
        print doc "/FontWeight %d " metrics.fontWeight;
        (match metrics.flags with Some x -> print doc "/Flags %d " x | _ -> ());
        print doc "/ItalicAngle %.2f " metrics.italicAngle;
        (match metrics.ascent with Some x -> print doc "/Ascent %d " x | _ -> ());
        (match metrics.descent with Some x -> print doc "/Descent %d " x | _ -> ());
        (match metrics.capHeight with Some x -> print doc "/CapHeight %d " x | _ -> ());
        (match metrics.stemV with Some x -> print doc "/StemV %d " x | _ -> ());
        (match metrics.missingWidth with Some x -> print doc "/MissingWidth %d " x | _ -> ());
        (match metrics.fontFile with
            | Some _ ->
              let fe =
                try List.find (fun fe -> fe.fe_metrics.fontName = name) doc.font_embed
                with Not_found ->
                  kprintf failwith "Cannot embed font %s" name
              in
              print doc "/FontFile%s %d 0 R " (match typ with `Type1 -> "" | _ -> "2") fe.fe_obj
            | _ -> ());
        print doc ">>endobj\n";
      | `Additional atyp -> failwith ("print_font (Additional " ^ atyp ^ ")")
  end doc.fonts

(** print_images *)
let print_images doc =
  let open Fpdf_images in
  (* reset($this->images);
  while(list($file,$info)=each($this->images)) *)
  let rec print_image image =
    new_obj doc;
    image.image_obj <- doc.current_object_number;
    print doc "<</Type /XObject /Subtype /Image ";
    print doc "/Width %d /Height %d " image.image_width image.image_height;
    if image.image_colorspace = "Indexed" then
      print doc "/ColorSpace [/Indexed /DeviceRGB %d %d 0 R] "
        ((String.length image.image_palette) / 3 - 1) (doc.current_object_number + 1)
    else begin
      print doc "/ColorSpace /%s " image.image_colorspace;
      if image.image_colorspace = "DeviceCMYK" then print doc "/Decode [1 0 1 0 1 0 1 0] "
    end;
    print doc "/BitsPerComponent %d " image.image_bits;
    (match image.image_f with Some f -> print doc "/Filter /%s " f | _ -> ());
    (match image.image_params with Some params -> print doc "/DecodeParms <<%s>> " params | _ -> ());
    (match image.image_trns with
      | Some trns when (List.length trns > 0) ->
        let trns =
          List.fold_left begin fun acc x ->
            let x = string_of_int x in
            acc ^ x ^ " " ^ x ^ " "
          end "" trns
        in
        print doc "/Mask [%s]\n" trns
      | _ -> ());
    if image.image_smask <> None then print doc "/SMask %d 0 R " (doc.current_object_number + 1);
    (* image_data *)
    print doc "/Length %d>>\n" (String.length image.image_data);
    print_stream image.image_data doc;
    image.image_data <- "";
    (*unset($this->images[$file]['data']);*)
    print doc "endobj\n";
    (* Soft mask *)
    (match image.image_smask with
      | Some image_smask ->
        let dp = sprintf "/Predictor 15 /Colors 1 /BitsPerComponent 8 /Columns %d" image.image_width in
        let smask = {
          image_width      = image.image_width;
          image_height     = image.image_height;
          image_colorspace = "DeviceGray";
          image_bits       = 8;
          image_f          = image.image_f;
          image_params     = Some dp;
          image_data       = image_smask;
          image_palette    = "";
          image_name       = "";
          image_trns       = None;
          image_smask      = None;
          image_obj        = -1;
          image_index      = -1;
        } in
        print_image smask
      | _ -> ());
    (* Palette *)
    if image.image_colorspace = "Indexed" then begin
      new_obj doc;
      let filter = if doc.compress then "/Filter /FlateDecode " else "" in
      let pal = if doc.compress then gz_compress image.image_palette else image.image_palette in
      print doc "<<%s/Length %d>>\n" filter (String.length pal);
      print_stream pal doc;
      print doc "endobj\n"
    end
  in
  Fpdf_images.Table.iter print_image doc.images;;

(** print_xobject_dict *)
let print_xobject_dict doc =
  let i = ref 0 in
  Fpdf_images.Table.iter begin fun img ->
    incr i;
    print doc "/I%d %d 0 R\n" !i img.image_obj;
  end doc.images

(** print_resource_dict *)
let print_resource_dict doc =
  print doc "\n/ProcSet [/PDF /Text /ImageB /ImageC /ImageI]\n";
  print doc "/Font <<";
  (** sss *)
  List.iter begin fun (_, f) ->
    print doc "/F%d %d 0 R " f.font_index f.font_n
  end doc.fonts;
  print doc ">>\n";
  print doc "/XObject <<";
  print_xobject_dict doc;
  print doc ">>\n"

let print_resources doc =
  (*print_fonts doc;*)
  print_images doc;
  (* Resource dictionary *)
  (find_object 2 doc).obj_offset <- doc.current_length;
  print doc "2 0 obj<<";
  print_resource_dict doc;
  print doc ">>endobj\n";
  List.iter (fun f -> f()) (List.rev doc.print_resources);
  doc.print_resources <- [];;

let print_open_actions doc =
  let actions =
    let first_page = 0 in
    match doc.zoomMode with
      | `Fullpage -> (`GoTo {dest_page = first_page; dest_display = `Fit}) :: doc.open_actions
      | `Fullwidth -> (`GoTo {dest_page = first_page; dest_display = `FitH None}) :: doc.open_actions
      | `Real -> (`GoTo {dest_page = first_page; dest_display = (`XYZ (None, None, 1.0))}) :: doc.open_actions
      | `Custom_zoom z -> (`GoTo {dest_page = first_page; dest_display = (`XYZ (None, None, z /. 100.))}) :: doc.open_actions
      | `Default_zoom -> doc.open_actions
  in
  let i = ref 0 in
  List.fold_left begin fun first action ->
    new_obj doc;
    let obj = doc.current_object_number in
    let next = if !i = List.length actions - 1 then "" else sprintf "/Next %d 0 R " (obj + 1) in
    print doc "<<%s" next;
    begin
      match action with
        | `GoTo {dest_page; dest_display} ->
          let pg_obj = (List.nth (List.rev doc.pages) dest_page).pg_obj in
          print doc "/S/GoTo/D[%d 0 R " pg_obj;
          begin
            match dest_display with
              | `XYZ (left, top, zoom) -> print doc "/XYZ %s %s %f]"
                (match left with Some x -> string_of_float x | _ -> "null")
                (match top with Some x -> string_of_float x | _ -> "null") zoom
              | `Fit -> print doc "/Fit]"
              | `FitH top -> print doc "/FitH %s]"
                (match top with Some x -> string_of_float x | _ -> "null")
              | `FitV left -> print doc "/FitV %s]"
                (match left with Some x -> string_of_float x | _ -> "null")
          end;
        | `ResetForm -> print doc "/S/ResetForm";
    end;
    print doc ">>endobj\n";
    incr i;
    match first with None -> Some obj | _ -> first
  end None actions
;;

(** begin_page *)
let begin_page ?orientation doc =
  let orientation = match orientation with
    | None -> doc.def_orientation
    | Some o -> o in
  doc.page <- doc.page + 1;
  (* Double sided pages *)
  if doc.double_sided && doc.page > 0 then begin
    let swap = doc.l_margin in
    doc.l_margin <- doc.r_margin;
    doc.r_margin <- swap
  end;
  (*  *)
  let new_page = {
    pg_buffer = Buffer.create 1000;
    pg_change_orientation = (orientation <> doc.def_orientation);
    pg_link = None;
    pg_annots = {annot_obj=[]; annot_func=[]};
    pg_obj = 0;
  } in
  doc.pages <- new_page :: doc.pages;
  doc.state <- Begin_page;
  doc.pos_x <- doc.l_margin;
  doc.pos_y <- doc.t_margin;
  doc.font_family <- None;
  if orientation <> doc.cur_orientation then begin
    (* Change orientation *)
    begin match orientation with
      | `Portrait ->
        doc.w_pt <- doc.fw_pt;
        doc.h_pt <- doc.fh_pt;
        doc.w <- doc.fw;
        doc.h <- doc.fh
      | `Landscape ->
        doc.w_pt <- doc.fh_pt;
        doc.h_pt <- doc.fw_pt;
        doc.w <- doc.fh;
        doc.h <- doc.fw
      end;
      doc.pageBreakTrigger <- doc.h -. doc.b_margin;
      doc.cur_orientation <- orientation
  end

let end_page doc = doc.state <- End_page

(** print_document *)
let print_document doc =
  print_header doc;
  print_fonts doc;
  print_pages doc;  (* 1 obj = Pages *)
  print_resources doc;
  (* Info *)
  new_obj doc;
  print doc "<<";
  print_info doc;
  print doc ">>endobj\n";
  (* OpenAction *)
  doc.open_action_obj <- print_open_actions doc;
  (* Catalog *)
  new_obj doc;
  print doc "<<";
  print_catalog doc;
  print doc ">>endobj\n";
  (* Cross-ref *)
  let start_xref = Int32.of_int doc.current_length in
  print doc "xref\n";
  print doc "0 %d\n" (doc.current_object_number + 1);
  print doc "0000000000 65535 f \n";
  List.iter (fun o -> print doc "%010ld 00000 n \n" (Int32.of_int o.obj_offset)) (List.rev doc.objects);
  (* Trailer *)
  print doc "trailer\n";
  print doc "<<\n";
  print_trailer doc;
  print doc ">>\n";
  (* Startxref *)
  print doc "startxref\n";
  print doc "%ld\n" start_xref;
  print doc "%%%%EOF\n";
  doc.state <- End_document
;;

let add_annot page func =
  page.pg_annots.annot_func <- func :: page.pg_annots.annot_func;;

let add_resource func doc = doc.print_resources <- func :: doc.print_resources
let current_object_number doc = doc.current_object_number

let add_catalog func doc = doc.print_catalog <- func :: doc.print_catalog

(** TODO: add_link *)
let add_link ~x ~y ~width ~height ~link () = ()

