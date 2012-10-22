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
open Printf
open PDFUtil
open Image

type state = Begin_document | End_page | Begin_page | End_document

type link = (float * float * float * float * link_dest) list
and link_dest = Uri of string | Internal of int

type obj = {mutable obj_offset : int}
and font = {
  font_index     : int; (* i *)
  font_type      : font_type;
  font_name      : string;
  mutable font_n : int;
  font_up        : int;
  font_ut        : int;
  font_cw        : (char -> int);
}
and font_type = Core | Type1 | TrueType | Additional of string
and font_file = {
  ff_name           : string;
  ff_info           : (string * string) list;
  mutable ff_number : int
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
  mutable annot_func : (unit -> int list) list
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
  mutable fonts                 : (Font.key * font) list;   (* array of used fonts [(key, font) list] *)
  mutable font_files            : font_file list;           (* array of font files *)
  mutable diffs                 : string list;              (* array of encoding differences *)
  mutable images                : Image.t list;             (* array of used images *)
  mutable links                 : (int * float) list;       (* array of internal links *)
  mutable font_family           : Font.family option;       (* current font family *)
  mutable font_style            : Font.style list;          (* current font style *)
  mutable underline             : bool;                     (* underlining flag *)
  mutable current_font          : font option;              (* current font info *)
  mutable font_size_pt          : float;                    (* current font size in points *)
  mutable font_size             : float;                    (* current font size in user unit *)
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
  | End_document -> failwith "PDF.open_document: document already closed."
  | _ -> doc.state <- End_page

let n_pages doc = List.length doc.pages

let string_of_style style =
  let bold = List.mem `Bold style in
  let italic = List.mem `Italic style in
  Printf.sprintf "%S%S"
    (if bold then "B" else "") (if italic then "I" else "")

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
  print doc "%%PDF-%d.%d\n" doc.pdfVersionMajor doc.pdfVersionMinor

let print_stream str doc =
  print doc "stream\n";
  print doc "%s\n" str;
  print doc "endstream\n"

let print_trailer doc =
  print doc "/Size %d\n" (doc.current_object_number + 1);
  print doc "/Root %d 0 R\n" doc.current_object_number; (* object_index[Catalog] *)
  print doc "/Info %d 0 R\n" (doc.current_object_number - 1) (* object_index[Info] *)

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
    print doc "/Creator %s" (pdf_string doc.creator)(*;
  print doc "/CreationDate %s" (pdf_string "D:20051226121800")*)

let print_catalog doc =
  print doc "/Type /Catalog ";
  print doc "/Pages 1 0 R ";
  (match doc.open_action_obj with Some x -> print doc "/OpenAction %d 0 R " x | _ -> ());
(*  begin
    try
      let first_page = List.nth doc.pages (n_pages doc - 1) in
      let first_page_obj = first_page.pg_obj in
      begin
        match doc.zoomMode with
          | `Fullpage -> print doc "/OpenAction [%d 0 R /Fit] " first_page_obj
          | `Fullwidth -> print doc "/OpenAction [%d 0 R /FitH null] " first_page_obj
          | `Real -> print doc "/OpenAction [%d 0 R /XYZ null null 1] " first_page_obj
          | `Custom_zoom z -> print doc "/OpenAction [%d 0 R /XYZ null null %f] " first_page_obj (z /. 100.)
          | `Default_zoom -> ()
      end;
    with Not_found -> ()
  end;*)
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
    new_obj doc;
    page.pg_obj <- doc.current_object_number;
    print doc "<</Type /Page ";
    print doc "/Parent 1 0 R ";
    if page.pg_change_orientation then print doc "/MediaBox [0 0 %.2f %.2f] " h_pt w_pt;
    print doc "/Resources 2 0 R ";
    (* Links (NOT IMPLEMEMTED) *)
    (*begin match page.pg_link with
      | None -> ()
      | Some page_links ->
        let annots = ref "/Annots [" in
        List.iter begin fun (x, y, w, h, dest) ->
          let rect = sprintf "%.2f %.2f %.2f %.2f" x y (x+.w) (y-.h) in
          annots := !annots ^ "<</Type /Annot /Subtype /Link /Rect [" ^ rect ^ "] /Border [0 0 0] ";
          begin match dest with
            | Uri uri ->
              annots := !annots ^ "/A <</S /URI /URI " ^ (pdf_string uri) ^ ">>>>"
            | Internal index ->
              let (linked_page, xx) = get_link index doc in
              let h = if page.pg_change_orientation then w_pt else h_pt in
              annots := sprintf "/Dest [%d 0 R /XYZ 0 %.2f null]>>"
                (1 + 2 * linked_page) (h -. xx *. doc.k)
          end;
          print doc "%s]\n" !annots;
        end page_links;
    end;*)
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
  print doc "/MediaBox [0 0 %.2f %.2f] " w_pt h_pt;
  print doc ">>endobj\n"

let get_font_path () = failwith "get_font_path" (*"../fpdf153/font"*)

let print_fonts doc =
  List.iter begin fun diff ->
    new_obj doc;
    print doc "<</Type /Encoding /BaseEncoding /WindAnsiEncoding /Differences [%s]>>endobj\n" diff;
  end doc.diffs;
  (*let mqr = get_magic_quotes_runtime(); set_magic_quotes_runtime(0) in*)
  List.iter begin fun ({ff_name = file; ff_info = info} as ff) ->
    (* Font file embedding *)
    new_obj doc;
    ff.ff_number <- doc.current_object_number;
    let filename = Filename.concat (get_font_path()) file in
    if not (Sys.file_exists filename) then failwith ("Font file \"" ^ filename ^ "\" not found.");
    let font = fread filename in
    let compressed = false in (* compress *)

    if compressed && (List.exists (fun (a, _) -> a = "length2") info) then begin
(*        let header = *)
    end;

(*    $compressed=(substr($file,-2)=='.z');
    if(!$compressed && isset($info['length2']))
    {
      $header=(ord($font{0})==128);
      if($header)
      {
        //Strip first binary header
        $font=substr($font,6);
      }
      if($header && ord($font{$info['length1']})==128)
      {
        //Strip second binary header
        $font=substr($font,0,$info['length1']).substr($font,$info['length1']+6);
      }
    }*)

    print doc "<</Length %d\n" (Buffer.length font);
    if compressed then print doc "/Filter /FlateDecode\n";
    print doc "/Length1 %s\n" (List.assoc "length1" info);
    try print doc "/Length2 %s /Length3 0\n" (List.assoc "length2" info);
    with Not_found -> ();
    print doc ">>\n";
    print_stream (Buffer.contents font) doc;
    print doc "endobj\n"
  end doc.font_files;
  List.iter begin fun (_, fnt) ->
    (* Font objects *)
    fnt.font_n <- doc.current_object_number + 1;
    let typ, name = fnt.font_type, fnt.font_name in
    match typ with
      | Core ->
        (* Standard font *)
        new_obj doc;
        print doc "<</Type /Font ";
        print doc "/BaseFont /%s " name;
        print doc "/Subtype /Type1 ";
        if name <> "Symbol" && name <> "ZapfDingbats" then
          print doc "/Encoding /WinAnsiEncoding ";
        print doc ">>endobj\n";
      | Type1 | TrueType -> failwith "print_font (Type1 | TrueType)"
(*  //Additional Type1 or TrueType font
  $this->_newobj();
  $this->_out('<</Type /Font');
  $this->_out('/BaseFont /'.$name);
  $this->_out('/Subtype /'.$type);
  $this->_out('/FirstChar 32 /LastChar 255');
  $this->_out('/Widths '.($this->n+1).' 0 R');
  $this->_out('/FontDescriptor '.($this->n+2).' 0 R');
  if($font['enc'])
  {
    if(isset($font['diff']))
      $this->_out('/Encoding '.($nf+$font['diff']).' 0 R');
    else
      $this->_out('/Encoding /WinAnsiEncoding');
  }
  $this->_out('>>');
  $this->_out('endobj');
  //Widths
  $this->_newobj();
  $cw=&$font['cw'];
  $s='[';
  for($i=32;$i<=255;$i++)
    $s.=$cw[chr($i)].' ';
  $this->_out($s.']');
  $this->_out('endobj');
  //Descriptor
  $this->_newobj();
  $s='<</Type /FontDescriptor /FontName /'.$name;
  foreach($font['desc'] as $k=>$v)
    $s.=' /'.$k.' '.$v;
  $file=$font['file'];
  if($file)
    $s.=' /FontFile'.($type=='Type1' ? '' : '2').' '.$this->FontFiles[$file]['n'].' 0 R';
  $this->_out($s.'>>');
  $this->_out('endobj');*)
      | Additional atyp -> failwith ("print_font (Additional " ^ atyp ^ ")")
(*        //Allow for additional types
        $mtd='_put'.strtolower($type);
        if(!let_exists($this,$mtd))
        $this->Error('Unsupported font type: '.$type);
        $this->$mtd($font);*)
  end doc.fonts

let print_images doc =
  let filter = if doc.compress then "/Filter /FlateDecode " else "" in
  (* reset($this->images);
  while(list($file,$info)=each($this->images)) *)
  let images = (Array.of_list (List.rev doc.images)) in
  for index = 0 to Array.length images - 1 do
    let image = images.(index) in
    new_obj doc;
    image.image_obj <- doc.current_object_number;
    print doc "<</Type /XObject\n";
    print doc "/Subtype /Image\n";
    print doc "/Width %d\n" image.image_width;
    print doc "/Height %d\n" image.image_height;
    if image.image_colorspace = "Indexed" then
      print doc "/ColorSpace [/Indexed /DeviceRGB %d %d 0 R]\n"
        ((String.length image.image_palette) / 3 - 1) (doc.current_object_number + 1)
    else begin
      print doc "/ColorSpace /%s\n" image.image_colorspace;
      if image.image_colorspace = "DeviceCMYK" then print doc "/Decode [1 0 1 0 1 0 1 0]\n"
    end;
    print doc "/BitsPerComponent %d\n" image.image_bits;
    (match image.image_f with Some f -> print doc "/Filter /%s\n" f | _ -> ());
    (match image.image_params with Some params -> print doc "%s\n" params | _ -> ());
    (match image.image_trns with Some trns when (List.length trns > 0) ->
        let trns = List.fold_left (fun acc x -> acc ^ x ^ " " ^ x ^ " " ) "" trns in
        print doc "/Mask [%s]\n" trns
      | _ -> ());
    (* image_data *)
    print doc "/Length %d>>\n" (String.length image.image_data);
    print_stream image.image_data doc;
    image.image_data <- "";
    (*unset($this->images[$file]['data']);*)
    print doc "endobj\n";
    (* Palette *)
    if image.image_colorspace = "Indexed" then begin
      new_obj doc;
      let pal = if doc.compress then gz_compress image.image_palette else image.image_palette in
      print doc "<<%s/Length %d>>\n" filter (String.length pal);
      print_stream pal doc;
      print doc "endobj\n"
    end
  done


(** print_xobject_dict *)
let print_xobject_dict doc =
  Array.iteri begin fun i img ->
    print doc "/I%d %d 0 R\n" (i + 1) img.image_obj;
  end (Array.of_list (List.rev doc.images))

(** print_resource_dict *)
let print_resource_dict doc =
  print doc "\n/ProcSet [/PDF /Text /ImageB /ImageC /ImageI]\n";
  print doc "/Font <<";
  List.iter begin fun (_, f) ->
    print doc "/F%d %d 0 R " f.font_index f.font_n
  end doc.fonts;
  print doc ">>\n";
  print doc "/XObject <<";
  print_xobject_dict doc;
  print doc ">>\n"

let print_resources doc =
  print_fonts doc;
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
    let first_page = n_pages doc - 1 in
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
          let pg_obj = (List.nth doc.pages dest_page).pg_obj in
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
  List.iter begin fun page ->
    page.pg_annots.annot_obj <- List.flatten (List.map (fun f -> f()) page.pg_annots.annot_func)
  end (List.rev doc.pages);
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

let add_annot doc func =
  let page = get_current_page doc in
  page.pg_annots.annot_func <- func :: page.pg_annots.annot_func;;

let add_resource func doc = doc.print_resources <- func :: doc.print_resources
let current_object_number doc = doc.current_object_number

let add_catalog func doc = doc.print_catalog <- func :: doc.print_catalog

(** TODO: add_link *)
let add_link ~x ~y ~width ~height ~link () = ()

