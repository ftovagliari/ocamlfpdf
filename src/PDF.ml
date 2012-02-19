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
open PDFTypes
open PDFDocument
open PDFPage

type t = PDFDocument.t

let set_author x doc = doc.author <- x
let author doc = doc.author

let set_title x doc = doc.title <- x
let title doc = doc.title

let set_double_sided x doc = doc.double_sided <- x
let double_sided doc = doc.double_sided

let scale doc = doc.k

let set_auto_page_break ?(margin=0.) auto doc =
  (* Set auto page break mode and triggering margin *)
  doc.auto_page_break <- auto;
(*    b_margin <- margin;*)
  doc.pageBreakTrigger <- (doc.h -. doc.b_margin)

let set_display_mode ?(layout=`Continuous) zoom doc =
  (* Set display mode in viewer *)
  doc.zoomMode <- zoom;
  doc.layoutMode <- layout

let set_compression x doc =
  if x then (invalid_arg "PDF.set_compression: Compression not yet implemented");
  doc.compress <- x

let set ?x ?y doc =
  begin match x with None -> () | Some x -> doc.pos_x <- if x >= 0. then x else doc.w +. x end;
  begin match y with None -> () | Some y -> doc.pos_y <- if y >= 0. then y else doc.h +. y end
let x doc = doc.pos_x
let y doc = doc.pos_y

(** create *)
let create ?(orientation=`Portrait) ?(m_unit=`Mm) ?(format=`A4) ~outchan () =
(*  //Some checks (LOCALE) $this->_dochecks();*)
  (* Scale factor *)
  let scale = match m_unit with
    | `Pt -> 1.
    | `Mm -> 72. /. 25.4
    | `Cm -> 72. /. 2.54
    | `In -> 72. in
  let default_fw_pt, default_fh_pt = match format with
    | `A3 -> 841.89, 1190.55
    | `A4 -> 595.28, 841.89
    | `A5 -> 420.94, 595.28
    | `Letter -> 612., 792.
    | `Legal -> 612., 1008.
    | `Custom_format (w, h) -> w *. scale, h *. scale in
  let default_orientation, default_w_pt, default_h_pt = match orientation with
    | `Portrait -> `Portrait, default_fw_pt, default_fh_pt
    | `Landscape -> `Landscape, default_fh_pt, default_fw_pt
  in
  (* Page margins (1 cm) *)
  let margin0 = 28.35 /. scale in
  let doc               = {
    page                  = -1;
    current_object_number = 2;
    objects               = [{obj_offset = -1}; {obj_offset = -1}];
    pages                 = [];
    state                 = Begin_document;
    compress              = false;
    def_orientation       = default_orientation;
    cur_orientation       = default_orientation;
    k                     = scale;
    fw_pt                 = default_fw_pt;
    fh_pt                 = default_fh_pt;
    fw                    = default_fw_pt /. scale;
    fh                    = default_fh_pt /. scale;
    w_pt                  = default_w_pt;
    h_pt                  = default_h_pt;
    w                     = default_w_pt /. scale;
    h                     = default_h_pt /. scale;
    l_margin              = 0.;
    t_margin              = 0.;
    r_margin              = 0.;
    b_margin              = 0.;
    c_margin              = margin0 /. 10.;
    pos_x                 = 0.;
    pos_y                 = 0.;
    lasth                 = 0.0;
    line_width            = 0.1;
    line_cap              = `Square;
    line_join             = `Miter;
    line_dash             = ([], 0);
    fonts                 = [];
    font_files            = [];
    diffs                 = [];
    images                = [];
    links                 = [];
    font_family           = Some `Courier;
    font_style            = [];
    underline             = false;
    current_font          = None;
    font_size_pt          = 12.;
    font_size             = 0.;
    drawColor             = "0 G";
    fillColor             = "0 g";
    textColor             = "0 g";
    colorFlag             = false;
    ws                    = 0.0;
    auto_page_break       = true;
    pageBreakTrigger      = 0.;
    inFooter              = false;
    zoomMode              = `Default_zoom;
    layoutMode            = `Default_layout;
    title                 = "";
    subject               = "";
    author                = "";
    keywords              = [];
    creator               = "";
    aliasNbPages          = "{nb}";
    pdfVersionMajor       = 1;
    pdfVersionMinor       = 3;
    header                = (fun () -> ());
    footer                = (fun () -> ());
    double_sided          = false;
    current_length        = 0;
    outchan               = outchan;
    print_resources       = [];
    print_catalog         = [];
    text_color_rgb        = (0, 0, 0);
    fill_color_rgb        = (0, 0, 0);
    draw_color_rgb        = (0, 0, 0);
  } in
  set_margins ~left:margin0 ~top:margin0 doc;
  (* Automatic page break *)
  set_auto_page_break true doc;
  (* Full width display mode *)
  set_display_mode `Fullwidth doc;
  (* Enable compression *)
  set_compression false doc;
  doc;;

let add_page            = PDFPage.add_page
let page_num            = PDFPage.page_num
let page_count          = PDFPage.page_count
let page_width          = PDFPage.page_width
let page_height         = PDFPage.page_height
let set_header_func     = PDFPage.set_header_func
let set_footer_func     = PDFPage.set_footer_func
let set_margins         = PDFPage.set_margins
let margins             = PDFPage.margins

let set_font            = PDFFont.set_font
let font_style          = PDFFont.font_style
let font_size           = PDFFont.font_size
let font_family         = PDFFont.font_family

let set_text_color      = PDFText.set_text_color
let text_color          = PDFText.text_color
let text                = PDFText.text
let newline             = PDFText.newline
let text                = PDFText.text
let write               = PDFText.write
let cell                = PDFText.cell
let multi_cell          = PDFText.multi_cell
let multi_cell_lines    = PDFText.multi_cell_lines
let get_string_width    = PDFText.get_string_width

let image               = PDFGraphics.image
let line                = PDFGraphics.line
let rect                = PDFGraphics.rect
let set_draw_color      = PDFGraphics.set_draw_color
let draw_color          = PDFGraphics.draw_color
let set_fill_color      = PDFGraphics.set_fill_color
let fill_color          = PDFGraphics.fill_color
let set_line_width      = PDFGraphicsState.set_line_width
let line_width          = PDFGraphicsState.line_width
let set_line_dash       = PDFGraphicsState.set_line_dash
let line_dash           = PDFGraphicsState.line_dash
let set_line_join       = PDFGraphicsState.set_line_join
let line_join           = PDFGraphicsState.line_join
let set_line_cap        = PDFGraphicsState.set_line_cap
let line_cap            = PDFGraphicsState.line_cap
let push_graphics_state = PDFGraphicsState.push
let pop_graphics_state  = PDFGraphicsState.pop

let close_document doc = match doc.state with
  | End_document -> ()
  | _ ->
    if n_pages doc = 0 then add_page doc;
    doc.inFooter <- true;
    doc.footer ();
    doc.inFooter <- false;
    print_document doc;
    end_page doc;;

