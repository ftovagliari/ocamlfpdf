(*

  OCaml-FPDF
  Copyright (C) 2010 Francesco Tovagliari

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

(*  *)

open PDFUtil
open Font
open Image
open Printf

type state = Begin_document | End_page | Begin_page | End_document
and orientation = Portrait | Landscape
and m_unit = Mm | Pt | Cm | In
and format = A3 | A4 | A5 | Letter | Legal | Custom_format of float * float
and zoom = Fullpage | Fullwidth | Real | Default_zoom | Custom_zoom of float
and layout = Single | Continuous | Two | Default_layout
and rect_style = [`Fill | `Both | `Outline]
and border = [ `All | `L | `B | `R | `T ] list
and align = [ `Left | `Justified | `Center | `Right ]
and link = (float * float * float * float * link_ref) list
and link_ref = Uri of string | Internal of int
and internal_link = int * float
and obj = {mutable obj_offset : int}
and font = {
  font_index : int; (* i *)
  font_type : font_type;
  font_name : string;
  mutable font_n : int;
  font_up : int;
  font_ut : int;
  font_cw : (char -> int)
}
and font_type = Core | Type1 | TrueType | Additional of string
and font_file = {
  ff_name : string;
  ff_info : (string * string) list;
  mutable ff_number : int
}
and page = {
  mutable pg_buffer : Buffer.t;
  mutable pg_change_orientation : bool;
  mutable pg_link : link option
}
type document = {
  mutable page : int;
  mutable current_object_number : int;
  mutable objects : obj list;
  mutable pages : page list;                     (* array containing pages *)
  mutable state : state;                         (* current document state *)
  mutable compress : bool;                       (* compression flag *)
  mutable def_orientation : orientation;         (* default orientation *)
  mutable cur_orientation : orientation;         (* current orientation *)
  mutable k : float;                             (* scale factor (number of points in user unit) *)
  mutable fw_pt : float;
  mutable fh_pt : float;                         (* dimensions of page format in points *)
  mutable fw : float;
  mutable fh : float;                            (* dimensions of page format in user unit *)
  mutable w_pt : float;
  mutable h_pt : float;                          (* current dimensions of page in points *)
  mutable w : float;
  mutable h : float;                             (* current dimensions of page in user unit *)
  mutable l_margin : float;                      (* left margin*)
  mutable t_margin : float;                      (* top margin*)
  mutable r_margin : float;                      (* right margin*)
  mutable b_margin : float;                      (* page break margin *)
  mutable c_margin : float;                      (* cell margin *)
  mutable pos_x : float;
  mutable pos_y : float;                         (* current position in user unit for cell positioning *)
  mutable lasth : float;                         (* height of last cell printed *)
  mutable line_width : float; (*0.1 /. scale*)   (* line width in user unit (0.567 = 0.2 mm) *)
  mutable fonts : (Font.key * font) list;        (* array of used fonts [(key, font) list] *)
  mutable font_files : font_file list;           (* array of font files *)
  mutable diffs : string list;                   (* array of encoding differences *)
  mutable images : Image.t list;                 (* array of used images *)
  mutable links : (int * float) list;            (* array of internal links *)
  mutable font_family : Font.family option;      (* current font family *)
  mutable font_style : Font.style list;          (* current font style *)
  mutable underline : bool;                      (* underlining flag *)
  mutable current_font : font option;            (* current font info *)
  mutable font_size_pt : float;                  (* current font size in points *)
  mutable font_size : float;                     (* current font size in user unit *)
  mutable drawColor : string;                    (* commands for drawing color *)
  mutable fillColor : string;                    (* commands for filling color *)
  mutable textColor : string;                    (* commands for text color *)
  mutable colorFlag : bool;                      (* indicates whether fill and text colors are different *)
  mutable ws : float;                            (* word spacing *)
  mutable auto_page_break : bool;                  (* automatic page breaking *)
  mutable pageBreakTrigger : float;              (* threshold used to trigger page breaks *)
  mutable inFooter : bool;                       (* flag set when processing footer *)
  mutable zoomMode : zoom;                       (* zoom display mode *)
  mutable layoutMode : layout;                   (* layout display mode *)
  mutable title : string;                        (* title *)
  mutable subject : string;                      (* subject *)
  mutable author : string;                       (* author *)
  mutable keywords : string list;                (* keywords *)
  mutable creator : string;                      (* creator *)
  mutable aliasNbPages : string;                 (* alias for total number of pages *)
  mutable pdfVersionMajor : int;                 (* PDF version number *)
  mutable pdfVersionMinor : int;                 (* PDF version number *)
  mutable header : (unit -> unit);
  mutable footer : (unit -> unit);
  mutable double_sided : bool;
  mutable current_length : int;
  mutable outchan : out_channel;
  mutable print_resources : (unit -> unit) list;
  mutable print_catalog : (unit -> unit) list;
  mutable text_color_rgb : int * int * int;
  mutable fill_color_rgb : int * int * int;
  mutable draw_color_rgb : int * int * int;
}

let set ?x ?y doc =
  begin match x with None -> () | Some x -> doc.pos_x <- if x >= 0. then x else doc.w +. x end;
  begin match y with None -> () | Some y -> doc.pos_y <- if y >= 0. then y else doc.h +. y end

let x doc = doc.pos_x

let y doc = doc.pos_y

let page_width doc = doc.w

let page_height doc = doc.h

let set_header_func callback doc = doc.header <- callback

let set_footer_func callback doc = doc.footer <- callback

let margins doc = doc.t_margin, doc.r_margin, doc.b_margin, doc.l_margin

let set_double_sided x doc = doc.double_sided <- x

let double_sided doc = doc.double_sided

let font_style doc = doc.font_style

let font_size doc = doc.font_size_pt

let font_family doc = doc.font_family

let set_author x doc = doc.author <- x
let author doc = doc.author

let set_title x doc = doc.title <- x
let title doc = doc.title

let get_font_path () = failwith "get_font_path" (*"../fpdf153/font"*)

let string_of_style style =
  let bold = List.mem `Bold style in
  let italic = List.mem `Italic style in
  Printf.sprintf "%S%S"
    (if bold then "B" else "") (if italic then "I" else "")

let set_margins ~left ?(right=left) ~top ?(bottom=top) doc =
  doc.l_margin <- left;
  doc.r_margin <- right;
  doc.t_margin <- top;
  doc.b_margin <- bottom

let set_auto_page_break ?(margin=0.) auto doc =
  (* Set auto page break mode and triggering margin *)
  doc.auto_page_break <- auto;
(*    b_margin <- margin;*)
  doc.pageBreakTrigger <- (doc.h -. doc.b_margin)

let set_display_mode ?(layout=Continuous) zoom doc =
  (* Set display mode in viewer *)
  doc.zoomMode <- zoom;
  doc.layoutMode <- layout

let set_compression x doc =
  if x then (invalid_arg "PDF.set_compression: Compression not yet implemented");
  doc.compress <- x

let create ?(orientation=Portrait) ?(m_unit=Mm) ?(format=A4) ~outchan () =
(*  //Some checks (LOCALE) $this->_dochecks();*)
  (* Scale factor *)
  let scale = match m_unit with
    | Pt -> 1.
    | Mm -> 72. /. 25.4
    | Cm -> 72. /. 2.54
    | In -> 72. in
  let default_fw_pt, default_fh_pt = match format with
    | A3 -> 841.89, 1190.55
    | A4 -> 595.28, 841.89
    | A5 -> 420.94, 595.28
    | Letter -> 612., 792.
    | Legal -> 612., 1008.
    | Custom_format (w, h) -> w *. scale, h *. scale in
  let default_orientation, default_w_pt, default_h_pt = match orientation with
    | Portrait -> Portrait, default_fw_pt, default_fh_pt
    | Landscape -> Landscape, default_fh_pt, default_fw_pt
  in
  (* Page margins (1 cm) *)
  let margin0 = 28.35 /. scale in
  let doc = {
    page = -1;
    current_object_number = 2;
    objects = [{obj_offset = -1}; {obj_offset = -1}];
    pages = [];
    state = Begin_document;
    compress = false;
    def_orientation = default_orientation;
    cur_orientation = default_orientation;
    k = scale;
    fw_pt = default_fw_pt;
    fh_pt = default_fh_pt;
    fw = default_fw_pt /. scale;
    fh = default_fh_pt /. scale;
    w_pt = default_w_pt;
    h_pt = default_h_pt;
    w = default_w_pt /. scale;
    h = default_h_pt /. scale;
    l_margin = 0.;
    t_margin = 0.;
    r_margin = 0.;
    b_margin = 0.;
    c_margin = margin0 /. 10.;
    pos_x = 0.;
    pos_y = 0.;
    lasth = 0.0;
    line_width =  0.1;
    fonts = [];
    font_files = [];
    diffs = [];
    images = [];
    links = [];
    font_family = Some `Courier;
    font_style = [];
    underline = false;
    current_font = None;
    font_size_pt = 12.;
    font_size = 0.;
    drawColor = "0 G";
    fillColor = "0 g";
    textColor = "0 g";
    colorFlag = false;
    ws = 0.0;
    auto_page_break = true;
    pageBreakTrigger = 0.;
    inFooter = false;
    zoomMode = Default_zoom;
    layoutMode = Default_layout;
    title = "";
    subject = "";
    author = "";
    keywords = [];
    creator = "";
    aliasNbPages = "{nb}";
    pdfVersionMajor = 1;
    pdfVersionMinor = 3;
    header = (fun () -> ());
    footer = (fun () -> ());
    double_sided = false;
    current_length = 0;
    outchan = outchan;
    print_resources = [];
    print_catalog = [];
    text_color_rgb = (0, 0, 0);
    fill_color_rgb = (0, 0, 0);
    draw_color_rgb = (0, 0, 0);
  } in
  set_margins ~left:margin0 ~top:margin0 doc;
  (* Automatic page break *)
  set_auto_page_break true doc;
  (* Full width display mode *)
  set_display_mode Fullwidth doc;
  (* Enable compression *)
  set_compression false doc;
  doc

let scale doc = doc.k

(** Number of pages of the document. *)
let n_pages doc = List.length doc.pages

let get_current_page doc =
  let len = List.length doc.pages in
  try
    List.nth doc.pages (len - doc.page - 1)
  with Failure "nth" -> kprintf failwith "get_current_page (%d, %d)" len doc.page

let page_no doc = doc.page

let get_page n doc =
  try
    let len = List.length doc.pages in
    List.nth doc.pages (len - n - 1)
  with Invalid_argument "List.nth" -> failwith ("get_page (" ^ (string_of_int n) ^ ")")

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

let find_image name doc =
  let equals = fun {Image.image_name = name'} -> name' = name in
  let rec find i = function
    | a :: b -> if equals a then (i, a) else find (i + 1) b
    | [] -> raise Not_found
  in
  find 1 doc.images

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
  print doc "/Producer %s\n" (pdf_string "OCaml-FPDF");
  if doc.title <> "" then
    print doc "/Title %s\n" (pdf_string doc.title);
  if doc.subject <> "" then
    print doc "/Subject %s\n" (pdf_string doc.subject);
  if doc.author <> "" then
    print doc "/Author %s\n" (pdf_string doc.author);
  if List.length doc.keywords <> 0 then
    print doc "/Keywords %s\n" (pdf_string (String.concat " " doc.keywords));
  if doc.creator <> "" then
    print doc "/Creator %s\n" (pdf_string doc.creator)(*;
  print doc "/CreationDate %s\n" (pdf_string "D:20051226121800")*)

let print_catalog doc =
  print doc "/Type /Catalog\n";
  print doc "/Pages 1 0 R\n";
  begin match doc.zoomMode with
    | Fullpage -> print doc "/OpenAction [3 0 R /Fit]\n"
    | Fullwidth -> print doc "/OpenAction [3 0 R /FitH null]\n"
    | Real -> print doc "/OpenAction [3 0 R /XYZ null null 1]\n"
    | Custom_zoom z -> print doc "/OpenAction [3 0 R /XYZ null null %f]\n" (z /. 100.)
    | _ -> () (*    | Default_zoom -> ""*)
  end;
  begin match doc.layoutMode with
    | Single -> print doc "/PageLayout /SinglePage\n"
    | Continuous -> print doc "/PageLayout /OneColumn\n"
    | Two -> print doc "/PageLayout /TwoColumnLeft\n"
    | _ -> () (*| Default_layout*)
  end;
  List.iter (fun f -> f()) (List.rev doc.print_catalog);
  doc.print_catalog <- []

(** Begin a new object *)
let new_obj doc =
   doc.current_object_number <- doc.current_object_number + 1;
   let obj = {obj_offset = doc.current_length} in
   doc.objects <- obj :: doc.objects;
   print doc "%d 0 obj\n" doc.current_object_number

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
    | Portrait -> doc.fw_pt, doc.fh_pt
    | Landscape -> doc.fh_pt, doc.fw_pt in
  let filter = if doc.compress then "/Filter /FlateDecode " else "" in
  List.iter begin fun page ->
    (* Page *)
    new_obj doc;
    print doc "<</Type /Page\n";
    print doc "/Parent 1 0 R\n";
    if page.pg_change_orientation then print doc "/MediaBox [0 0 %.2f %.2f]\n" h_pt w_pt;
    print doc "/Resources 2 0 R\n";
    (* Links *)
    begin match page.pg_link with
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
    end;
    print doc "/Contents %d 0 R>>\n" (doc.current_object_number + 1);
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
  print doc "1 0 obj\n";
  print doc "<</Type /Pages\n";
  print doc "/Kids [";
  for i = 0 to nb do print doc "%d 0 R " (3 + 2 * i) done;
  print doc "]\n";
  print doc "/Count %d\n" (nb + 1);
  print doc "/MediaBox [0 0 %.2f %.2f]\n" w_pt h_pt;
  print doc ">>\n";
  print doc "endobj\n"

let print_fonts doc =
  List.iter begin fun diff ->
    new_obj doc;
    print doc "<</Type /Encoding /BaseEncoding /WindAnsiEncoding /Differences [%s]>>\n" diff;
    print doc "endobj\n"
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
        print doc "<</Type /Font\n";
        print doc "/BaseFont /%s\n" name;
        print doc "/Subtype /Type1\n";
        if name <> "Symbol" && name <> "ZapfDingbats" then
          print doc "/Encoding /WinAnsiEncoding\n";
        print doc ">>\n";
        print doc "endobj\n";
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
    new_obj doc; (* Incrementa current_object_number *)
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
  print doc "/ProcSet [/PDF /Text /ImageB /ImageC /ImageI]\n";
  print doc "/Font <<\n";
  List.iter begin fun (_, f) ->
    print doc "/F%d %d 0 R\n" f.font_index f.font_n
  end doc.fonts;
  print doc ">>\n";
  print doc "/XObject <<\n";
  print_xobject_dict doc;
  print doc ">>\n"

let print_resources doc =
  print_fonts doc;
  print_images doc;
  (* Resource dictionary *)
  (find_object 2 doc).obj_offset <- doc.current_length;
  print doc "2 0 obj\n";
  print doc "<<\n";
  print_resource_dict doc;
  print doc ">>\n";
  print doc "endobj\n";
  List.iter (fun f -> f()) (List.rev doc.print_resources);
  doc.print_resources <- []

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
    pg_link = None} in
  doc.pages <- new_page :: doc.pages;
  doc.state <- Begin_page;
  doc.pos_x <- doc.l_margin;
  doc.pos_y <- doc.t_margin;
  doc.font_family <- None;
  if orientation <> doc.cur_orientation then begin
    (* Change orientation *)
    begin match orientation with
      | Portrait ->
        doc.w_pt <- doc.fw_pt;
        doc.h_pt <- doc.fh_pt;
        doc.w <- doc.fw;
        doc.h <- doc.fh
      | Landscape ->
        doc.w_pt <- doc.fh_pt;
        doc.h_pt <- doc.fw_pt;
        doc.w <- doc.fh;
        doc.h <- doc.fw
      end;
      doc.pageBreakTrigger <- doc.h -. doc.b_margin;
      doc.cur_orientation <- orientation
  end

(** End of page contents *)
let end_page doc = doc.state <- End_page

(** TODO *)
let do_underline x y txt = ""

let print_document doc =
  print_header doc;
  print_pages doc;  (* 1 obj *)
  print_resources doc; (* 2 obj *)
  (* Info *)
  new_obj doc;
  print doc "<<\n";
  print_info doc;
  print doc ">>\n";
  print doc "endobj\n";
  (* Catalog *)
  new_obj doc;
  print doc "<<\n";
  print_catalog doc;
  print doc ">>\n";
  print doc "endobj\n";
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
(** set_font *)
let set_font ?family ?(style=([] : Font.style list)) ?size (*?(size=0.)*) doc =
  let family = match (family : Font.family option) with None -> doc.font_family | x -> x in
  (* Select a font; size given in points. *)
  doc.underline <- List.mem `Underline style;
  let size = match size with None -> doc.font_size_pt | Some x -> x in (*if size = 0. then font_size_pt else size in*)
  (* Test if font is already selected *)
  if doc.font_family <> family || doc.font_style <> style || doc.font_size_pt <> size then begin
    (* Test if font is used for the first time *)
    let fkey = Font.key_of_font style (match family with None -> assert false | Some f -> f) in
    if not (font_exists fkey doc) then begin
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
    let current_font = List.assoc fkey doc.fonts in
    doc.current_font <- Some current_font;
    if n_pages doc > 0 then
      print_buffer doc "BT /F%d %.2f Tf ET\n" current_font.font_index doc.font_size_pt
  end

(** open_document *)
let open_document doc = match doc.state with
  | End_document -> failwith "PDF.open_document: document already closed."
  | _ -> doc.state <- End_page

(** set_line_width *)
let set_line_width width doc =
  doc.line_width <- width;
  if doc.page >= 0 then (print_buffer doc "%.2f w\n" (width *. doc.k))

(** line_width *)
let line_width doc = doc.line_width

(** Start a new page *)
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
      print_buffer doc "%.2f w\n" (lw *. doc.k);
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
  print_buffer doc "2 J\n";
  call_and_restore ~pre:save doc.header () ~post:restore;
  set_line_width old_lw doc

let close_document doc = match doc.state with
  | End_document -> ()
  | _ ->
    if n_pages doc = 0 then add_page doc;
    doc.inFooter <- true;
    doc.footer ();
    doc.inFooter <- false;
    print_document doc;
    end_page doc

let text ~x ~y ~text doc =
  let s = ref (sprintf "BT %.2f %.2f Td (%s) Tj ET"
    (x *. doc.k) ((doc.h -. y) *. doc.k) (escape text)) in
  if doc.underline  && text <> "" then s := !s ^ " " ^ (do_underline x y text);
  if doc.colorFlag then s:= "q " ^ doc.textColor ^ " " ^ !s ^ " Q";
  print_buffer doc "%s\n" !s

(** TODO: add_link *)
let add_link ~x ~y ~width ~height ~link () = ()

let get_string_width s doc =
  let font = match doc.current_font with
    | None -> failwith "get_string_width: no current font."
    | Some f -> f in
  let cw = font.font_cw in
  let width = ref 0 in
  String.iter begin fun c ->
    width := !width + (cw c)
  end s;
  (float !width) *. doc.font_size /. 1000.

let set_draw_color ~red ?(green=(-1)) ?(blue=(-1)) doc =
  if (red = 0 && green = 0 && blue = 0) || green = -1 then
    (doc.drawColor <- sprintf "%.3f G" ((float red) /. 255.))
  else begin doc.drawColor <- sprintf "%.3f %.3f %.3f RG"
    ((float red) /. 255.) ((float green) /. 255.) ((float blue) /. 255.)
  end;
  if doc.page >= 0 then print_buffer doc "%s\n" doc.drawColor;
  doc.draw_color_rgb <- (red, green, red)

let draw_color doc = doc.draw_color_rgb

let set_fill_color ~red ?(green=(-1)) ?(blue=(-1)) doc =
  if (red = 0 && green = 0 && blue = 0) || green = -1 then
    (doc.fillColor <- sprintf "%.3f g" ((float red) /. 255.))
  else begin doc.fillColor <- sprintf "%.3f %.3f %.3f rg"
    ((float red) /. 255.) ((float green) /. 255.) ((float blue) /. 255.)
  end;
  doc.colorFlag <- doc.fillColor <> doc.textColor;
  if doc.page >= 0 then print_buffer doc "%s\n" doc.fillColor;
  doc.fill_color_rgb <- (red, green, red)

let fill_color doc = doc.fill_color_rgb

let set_text_color ~red ?(green=(-1)) ?(blue=(-1)) doc =
  if (red = 0 && green = 0 && blue = 0) || green = -1 then
    (doc.textColor <- sprintf "%.3f g" ((float red) /. 255.))
  else begin doc.textColor <- sprintf "%.3f %.3f %.3f rg"
    ((float red) /. 255.) ((float green) /. 255.) ((float blue) /. 255.)
  end;
  doc.colorFlag <- doc.fillColor <> doc.textColor;
  doc.text_color_rgb <- (red, green, red)

let text_color doc = doc.text_color_rgb

(** line *)
let line ~x1 ~y1 ~x2 ~y2 doc =
  print_buffer doc "%.2f %.2f m %.2f %.2f l S\n" (x1 *. doc.k) ((doc.h -. y1) *. doc.k) (x2 *. doc.k) ((doc.h -. y2) *. doc.k)

let newline ?height doc =
  doc.pos_x <- doc.l_margin;
  match height with
    | None -> doc.pos_y <- doc.pos_y +. doc.lasth
    | Some h -> doc.pos_y <- doc.pos_y +. h

let cell
    ~width
    ?(height=0.)
    ?(text="")
    ?(border : border option)
    ?padding
    ?(ln=(`Right : [`Right | `Next_line | `Bottom]))
    ?(align=`Left)
    ?(fill=false)
    ?(link="")
    doc =
  let padding = match padding with None -> doc.c_margin | Some padding -> padding in
  let scale = doc.k in
  if doc.pos_y +. height > doc.pageBreakTrigger && (not doc.inFooter) && (doc.auto_page_break) then begin
    let x0 = doc.pos_x in
    let word_spacing = doc.ws in
    if word_spacing > 0. then begin
      doc.ws <- 0.;
      print_buffer doc "0 Tw\n"
    end;
    add_page ~orientation:doc.cur_orientation doc;
    doc.pos_x <- x0;
    if word_spacing > 0. then begin
      doc.ws <- word_spacing;
      print_buffer doc "%.3f Tw\n" (word_spacing *. scale)
    end;
  end;
  let text_width = get_string_width text in
  let width = if width = 0. then doc.w -. doc.r_margin -. doc.pos_x else width in
  (* Frame *)
  let border, frame = match border with
    | None -> [], false
    | Some b -> if List.mem `All b then [`All], true else b, false in
  if fill || frame then begin
    let op = if fill && frame then "B"
      else if fill && not frame then "f"
      else "S" in (* frame && not fill *)
    print_buffer doc "%.2f %.2f %.2f %.2f re %s "
      (doc.pos_x *. scale) ((doc.h -. doc.pos_y) *. scale) (width *. scale) (-.(height) *. scale) op
  end;
  (* The code string *)
  let code = ref "" in
  (* Borders *)
  if not frame then begin
    let border = List.sort compare (remove_dupl border) in
    let x0, y0 = doc.pos_x, doc.pos_y in
    let sprintf = sprintf "%.2f %.2f m %.2f %.2f l S " in
    let border_code = List.map begin function
      | `L -> sprintf (x0 *. scale) ((doc.h -. y0) *. scale) (x0 *. scale) ((doc.h -. (y0 +. height)) *. scale)
      | `T -> sprintf (x0 *. scale) ((doc.h -. y0) *. scale) ((x0 +. width) *. scale) ((doc.h -. y0) *. scale)
      | `R -> sprintf ((x0 +. width) *. scale) ((doc.h -. y0) *. scale) ((x0 +. width) *. scale)
        ((doc.h -. (y0 +. height)) *. scale)
      | `B -> sprintf (x0 *. scale) ((doc.h -. (y0 +. height)) *. scale)
        ((x0 +. width) *. scale) ((doc.h -. (y0 +. height)) *. scale)
      | _ -> ""
    end border in
    code := String.concat "" border_code
  end;
  (* Text *)
  (*let posy = doc.pos_y +. 0.5 *. height +. 0.3 *. doc.font_size in*)
  let posy = doc.pos_y +. 0.75 *. height in
  if (String.length text) > 0 then begin
    let dx = match align with
      | `Left | `Justified -> padding
      | `Center -> (width -. text_width doc) /. 2.
      | `Right -> width -. padding -. text_width doc in
    if doc.colorFlag then code := !code ^ "q " ^ doc.textColor ^ " ";
    code := !code ^ (sprintf "BT %.2f %.2f Td (%s) Tj ET" ((doc.pos_x +. dx) *. scale)
      ((doc.h -. posy) *. scale) (escape text));
    if doc.underline then code := !code ^
      (" " ^ (do_underline (doc.pos_x +. dx) posy text));
    if doc.colorFlag then code := !code ^ " Q";
    if (String.length link) > 0 then
      add_link (doc.pos_x +. dx) posy text_width doc.font_size link ();
  end;
  if (String.length !code) > 0 then print_buffer doc "%s\n" !code;
  doc.lasth <- height;
  (* ln *)
  match ln with
    | `Right -> doc.pos_x <- doc.pos_x +. width
    | `Bottom -> doc.pos_y <- doc.pos_y +. height
    | `Next_line ->
      doc.pos_y <- doc.pos_y +. height;
      doc.pos_x <- doc.l_margin

(** multi_cell *)
let multi_cell' ~width ~line_height ~text ?border ?padding ?(align=(`Left : align)) ?(fill = false) ?(printing=true) doc =
  let height = line_height in
  let padding = match padding with None -> doc.c_margin | Some x -> x in
  let cw = match doc.current_font with None -> failwith "multi_cell: no current font."
    | Some f -> f.font_cw in
  let text_lines = ref [] in
  let width = if width = 0. then doc.w -. doc.r_margin -. doc.pos_x else width in
  let wmax = (width -. 2. *. padding) *. 1000. /. doc.font_size in
  let text = Str.global_replace re_cr "" text in
  let nb = String.length text in
  let nb = if nb > 0 && text.[nb - 1] = '\n' then nb - 1 else nb in
  let old_ws = ref doc.ws in
  let old_pos_x = ref doc.pos_x in
  let old_pos_y = ref doc.pos_y in
  let old_lasth = ref doc.lasth in
  let border, b, b2 = match border with
    | None -> [], ref [], []
    | Some border ->
      let border = List.sort compare (remove_dupl border) in
      if List.mem `All border then [`L; `T; `R; `B], ref [`L; `R; `T;], [`L; `R]
      else
        let b2 = List.filter (function `L | `R -> true | _ -> false) border in
        let b = if List.mem `T border then `T :: b2 else b2 in
        border, ref b, b2 in
  let sep, i, j, l, ns, nl = ref (-1), ref 0, ref 0, ref 0, ref 0, ref 1 in
  let ls = ref 0 in
  while !i < nb do
    let c = text.[!i] in
    if c = '\n' then begin
      if doc.ws > 0. then begin
        doc.ws <- 0.;
        if printing then (print_buffer doc "0 Tw\n");
      end;
      let text = String.sub text !j (!i - !j) in
      if printing then (cell ~width ~height ~text ~border:(!b) ~padding ~ln:`Bottom ~align ~fill doc);
      text_lines := text :: !text_lines;
      incr i;
      sep := -1;
      j := !i;
      l := 0;
      ns := 0;
      incr nl;
      if (border <> []) && !nl = 2 then b := b2;
      (*raise Continue*)
    end else if c = ' ' then begin
      sep := !i;
      ls := !l;
      incr ns
    end;
    l := !l + (cw c);
    if (float !l) > wmax then begin
      (* Automatic line break *)
      if !sep = -1 then begin
        if !i = !j then incr i;
        if doc.ws > 0. then begin
          doc.ws <- 0.;
          if printing then (print_buffer doc "0 Tw\n");
        end;
        let text_line = String.sub text !j (!i - !j) in
        if printing then (cell ~width ~height ~text:text_line ~border:(!b) ~padding ~ln:`Bottom ~align ~fill doc);
        text_lines := text_line :: !text_lines;
      end else begin
        if align = `Justified then begin
          doc.ws <- if !ns > 1 then
            (wmax -. (float !ls)) /. 1000. *. doc.font_size /. (float (!ns - 1)) else 0.;
          if printing then (print_buffer doc "%.3f Tw\n" (doc.ws *. doc.k))
        end;
        let text_line = String.sub text !j (!sep - !j) in
        if printing then (cell ~width ~height ~text:text_line ~border:(!b) ~padding ~ln:`Bottom ~align ~fill doc);
        text_lines := text_line :: !text_lines;
        i := !sep + 1;
      end;
      sep := -1;
      j := !i;
      l := 0;
      ns := 0;
      incr nl;
      if (border <> []) && !nl = 2 then b := b2;
    end else incr i
  done;
  (* Last chunk *)
  if doc.ws > 0. then begin
    doc.ws <- 0.;
    if printing then (print_buffer doc "0 Tw\n")
  end;
  if (border <> []) && (List.mem `B border) then b := `B :: !b;
  let text_line = String.sub text !j (!i - !j) in
  if printing then (cell ~width ~height ~text:text_line ~border:(!b) ~padding ~ln:`Bottom ~align ~fill doc);
  text_lines := text_line :: !text_lines;
  if printing then (doc.pos_x <- doc.l_margin) else begin
    doc.ws <- !old_ws;
    doc.pos_x <- !old_pos_x;
    doc.pos_y <- !old_pos_y;
    doc.lasth <- !old_lasth;
  end;
  List.rev !text_lines

let multi_cell ~width ~line_height ~text ?border ?padding ?align ?fill doc =
  ignore (multi_cell' ~width ~line_height ~text ?border ?padding ?align ?fill ~printing:true doc)

let multi_cell_lines ~width ~line_height ~text ?border ?padding ?align ?fill doc =
  multi_cell' ~width ~line_height ~text ?border ?padding ?align ?fill ~printing:false doc

(** name ::= image_name.image_type, i.e. "myimage.jpg" *)
let image ~name ~data ~x ~y ~image_width ~image_height ?(width=0.) ?(height=0.) ?format ?link doc =
  let index, info =
    try find_image name doc
    with Not_found -> begin
      let info = Image.parse ?format ~width:image_width ~height:image_height name data in
      doc.images <- info :: doc.images;
      (List.length doc.images), info
    end in
(*    let width, height = swidth, sheight in*)
    let width, height = match width, height with
      | 0., 0. -> ((float info.image_width) /. doc.k), ((float info.image_height) /. doc.k)
      | _ -> width, height in
    let width = if width = 0. then height *. (float info.image_width) /. (float info.image_height)
      else width in
    let height = if height = 0. then width *. (float info.image_height) /. (float info.image_width)
      else height in
    print_buffer doc "q %.2f 0 0 %.2f %.2f %.2f cm /I%d Do Q\n" (width *. doc.k) (height *. doc.k)
      (x *. doc.k) ((doc.h -. (y +. height)) *. doc.k) index;
    match link with
      | None -> ()
      | Some l -> add_link ~x ~y ~width ~height ~link:l ()

let rect ?x ?y ~width ~height ?radius ?(style=(`Outline : rect_style)) doc =
  let x = match x with None -> doc.pos_x | Some x -> x in
  let y = match y with None -> doc.pos_y | Some y -> y in
  let op = match style with
    | `Fill -> 'f'
    | `Both -> 'B'
    | `Outline -> 'S' in
  match radius with
    | None -> print_buffer doc "%.2f %.2f %.2f %.2f re %c\n"
      (x *. doc.k) ((doc.h -. y) *. doc.k)
      (width *. doc.k) ((-.height) *. doc.k) op
    | Some radius ->
      let scale = doc.k in
      let arc x1 y1 x2 y2 x3 y3 = print_buffer doc "%.2f %.2f %.2f %.2f %.2f %.2f c \n"
        (x1 *. scale) ((doc.h -. y1) *. scale)
        (x2 *. scale) ((doc.h -. y2) *. scale)
        (x3 *. scale) ((doc.h -. y3) *. scale) in
      let hp = doc.h in
      let my_arc = 4./.3. *. (sqrt 2. -. 1.) in
      print_buffer doc "%.2f %.2f m\n" ((x +. radius) *. scale) ((hp -. y) *. scale);
      let xc = x +. width -. radius in
      let yc = y +. radius in
      print_buffer doc "%.2f %.2f l\n" (xc *. scale) ((hp -. y) *. scale);
      arc (xc +. radius *. my_arc) (yc -. radius) (xc +. radius) (yc -. radius *. my_arc) (xc +. radius) yc;
      let xc = x +. width -. radius in
      let yc = y +. height -. radius in
      print_buffer doc "%.2f %.2f l\n" ((x +. width) *. scale) ((hp -. yc) *. scale);
      arc (xc +. radius) (yc +. radius *. my_arc) (xc +. radius *. my_arc) (yc +. radius) xc (yc +. radius);
      let xc = x +. radius in
      let yc = y +. height -. radius in
      print_buffer doc "%.2f %.2f l\n" (xc *. scale) ((hp -. (y +. height)) *. scale);
      arc (xc -. radius *. my_arc) (yc +. radius) (xc -. radius) (yc +. radius *. my_arc) (xc -. radius) yc;
      let xc = x +. radius in
      let yc = y +. radius in
      print_buffer doc "%.2f %.2f l\n" (x *. scale) ((hp -. yc) *. scale);
      arc (xc -. radius) (yc -. radius *. my_arc) (xc -. radius *. my_arc) (yc -. radius) xc  (yc -. radius);
      print_buffer doc "%c\n" op

(* Output text in flowing mode*)
let write ~height ?padding ~text ?link doc =
  let padding = match padding with None -> doc.c_margin | Some padding -> padding in
  let cw = match doc.current_font with None -> assert false | Some font -> font.font_cw in
  let width = ref (doc.w -. doc.r_margin -. doc.pos_x) in
  let wmax = ref ((!width -. 2. *. padding) *. 1000. /. doc.font_size) in
  let text = Str.global_replace re_cr "" text in
  let nb = String.length text in
  let sep = ref (-1) and i = ref 0 and j = ref 0 and l = ref 0.0 and nl = ref 1 in
  while !i < nb do
    try
      let c = text.[!i] in
      if c = '\n' then begin (* Explicit line break *)
        cell ~width:!width ~height ~text:(String.sub text !j (!i - !j)) ~ln:`Bottom ?link doc;
        incr i;
        sep := -1;
        j := !i;
        l := 0.0;
        if !nl = 1 then begin
          doc.pos_x <- doc.l_margin;
          width := doc.w -. doc.r_margin -. doc.pos_x;
          wmax := (!width -. 2. *. padding) *. 1000. /. doc.font_size;
        end;
        incr nl;
        raise Continue;
      end;
      if c = ' ' then (sep := !i);
      l := !l +. float (cw c);
      if !l > !wmax then begin (* Automatic line break *)
        if !sep = (-1) then begin
          if doc.pos_x > doc.l_margin then begin (* Move to next line *)
            doc.pos_x <- doc.l_margin;
            doc.pos_y <- doc.pos_y +. height;
            width := doc.w -. doc.r_margin -. doc.pos_x;
            wmax := (!width -. 2. *. padding) *. 1000. /. doc.font_size;
            incr i;
            incr nl;
            raise Continue;
          end;
          if !i = !j then (incr i);
          cell ~width:!width ~height ~text:(String.sub text !j (!i - !j)) ~ln:`Bottom ?link doc;
        end else begin
          cell ~width:!width ~height ~text:(String.sub text !j (!sep - !j)) ~ln:`Bottom ?link doc;
          i := !sep + 1;
        end;
        sep := -1;
        j := !i;
        l := 0.0;
        if !nl = 1 then begin
          doc.pos_x <- doc.l_margin;
          width := doc.w -. doc.r_margin -. doc.pos_x;
          wmax := (!width -. 2. *. padding) *. 1000. /. doc.font_size;
        end;
        incr nl;
      end else (incr i);
    with Continue -> ()
  done;
  (* Last chunk *)
  if !i <> !j then begin
    cell ~width:(!l /. 1000. *. doc.font_size) ~height ~text:(String.sub text !j (String.length text - !j)) ~ln:`Right ?link doc;
  end

let add_resource func doc = doc.print_resources <- func :: doc.print_resources
let current_object_number doc = doc.current_object_number

let add_catalog func doc = doc.print_catalog <- func :: doc.print_catalog


