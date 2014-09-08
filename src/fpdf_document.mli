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

type state = Begin_document | End_page | Begin_page | End_document
type link = (float * float * float * float * link_dest) list
and link_dest = Uri of string | Internal of int
type obj = { mutable obj_offset : int; }
and font_obj = {
  font_metrics : Font.t;
  mutable font_n : int;
  mutable font_index : int;
}
and font_embed = { fe_metrics : Font.t; mutable fe_obj : int; }
and page = {
  mutable pg_buffer : Buffer.t;
  mutable pg_change_orientation : bool;
  mutable pg_link : link option;
  mutable pg_annots : annot;
  mutable pg_obj : int;
}
and annot = {
  mutable annot_obj : int list;
  mutable annot_func : (int -> int) list;
}
and t = {
  mutable page : int;
  mutable current_object_number : int;
  mutable objects : obj list;
  mutable pages : page list;
  mutable state : state;
  mutable compress : bool;
  mutable def_orientation : Fpdf_types.orientation;
  mutable cur_orientation : Fpdf_types.orientation;
  mutable k : float;
  mutable fw_pt : float;
  mutable fh_pt : float;
  mutable fw : float;
  mutable fh : float;
  mutable w_pt : float;
  mutable h_pt : float;
  mutable w : float;
  mutable h : float;
  mutable l_margin : float;
  mutable t_margin : float;
  mutable r_margin : float;
  mutable b_margin : float;
  mutable c_margin : float;
  mutable pos_x : float;
  mutable pos_y : float;
  mutable lasth : float;
  mutable line_width : float;
  mutable line_cap : Fpdf_types.line_cap_style;
  mutable line_join : Fpdf_types.line_join_style;
  mutable line_dash : int list * int;
  mutable fonts : (Font.key * font_obj) list;
  mutable font_embed : font_embed list;
  mutable diffs : string list;
  mutable images : Fpdf_images.Table.t;
  mutable links : (int * float) list;
  mutable font_family : Font.family option;
  mutable font_style : Font.style list;
  mutable underline : bool;
  mutable current_font : font_obj option;
  mutable font_size_pt : float;
  mutable font_size : float;
  mutable font_scale : int option;
  mutable font_char_space : float option;
  mutable drawColor : string;
  mutable fillColor : string;
  mutable textColor : string;
  mutable colorFlag : bool;
  mutable ws : float;
  mutable auto_page_break : bool;
  mutable pageBreakTrigger : float;
  mutable inFooter : bool;
  mutable zoomMode : Fpdf_types.zoom;
  mutable layoutMode : Fpdf_types.layout;
  mutable title : string;
  mutable subject : string;
  mutable author : string;
  mutable keywords : string list;
  mutable creator : string;
  mutable creation_date : string;
  mutable aliasNbPages : string;
  mutable pdfVersionMajor : int;
  mutable pdfVersionMinor : int;
  mutable header : unit -> unit;
  mutable footer : unit -> unit;
  mutable double_sided : bool;
  mutable current_length : int;
  mutable outchan : out_channel;
  mutable print_resources : (unit -> unit) list;
  mutable print_catalog : (unit -> unit) list;
  mutable text_color_rgb : int * int * int;
  mutable fill_color_rgb : int * int * int;
  mutable draw_color_rgb : int * int * int;
  mutable open_action_obj : int option;
  mutable open_actions : Fpdf_types.action list;
}
val open_document : t -> unit
val n_pages : t -> int
val string_of_style : [> `Bold | `Italic ] list -> string
val find_font :
  ?family:Font.family -> style:Font.style list -> t -> font_obj option
val get_current_page : t -> page
val get_link : int -> t -> int * float
val find_object : int -> t -> obj
val get_buffer : create:'a -> t -> Buffer.t
val font_exists : Font.key -> t -> bool
val print_buffer : t -> ('a, unit, string, unit) format4 -> 'a
val print : t -> ('a, unit, string, unit) format4 -> 'a
val print_header : t -> unit
val print_stream : string -> t -> unit
val print_trailer : t -> unit
val print_info : t -> unit
val print_catalog : t -> unit
val new_obj : t -> unit
val print_pages : t -> unit
val get_font_path : unit -> 'a
val print_fonts : t -> unit
val print_images : t -> unit
val print_xobject_dict : t -> unit
val print_resource_dict : t -> unit
val print_resources : t -> unit
val print_open_actions : t -> int option
val begin_page : ?orientation:Fpdf_types.orientation -> t -> unit
val end_page : t -> unit
val print_document : t -> unit
val add_annot : page -> (int -> int) -> unit
val add_resource : (unit -> unit) -> t -> unit
val current_object_number : t -> int
val add_catalog : (unit -> unit) -> t -> unit
val add_link :
  x:'a -> y:'b -> width:'c -> height:'d -> link:'e -> unit -> unit
