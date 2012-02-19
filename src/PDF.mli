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

(** PDF Document. *)
type link = (float * float * float * float * link_ref) list
and link_ref = Uri of string | Internal of int
and internal_link = int * float

type document

(** {4 PDF Document} *)

(**
  @param m_unit Default is [Mm].
  @param orientation Default is [Portrait].
  @param format Default is [A4].
  @param outchan The output channel where to send the document data.
  *)
val create :
  ?orientation:orientation ->
  ?m_unit:m_unit -> ?format:format -> outchan:out_channel -> unit -> document

(** Append a new page to the document.
    @param orientation Default value is the document orientation passed to [create].
  *)
val add_page : ?orientation:orientation -> document -> unit

val n_pages : document -> int

(** Close the PDF document.
    After this point, no further operation is possible on the document. *)
val close_document : document -> unit

val double_sided : document -> bool
val scale : document -> float
val set_double_sided : bool -> document -> unit
val set_auto_page_break : ?margin:float -> bool -> document -> unit
val set_display_mode : ?layout:layout -> zoom -> document -> unit

val author : document -> string
val title : document -> string
val set_author : string -> document -> unit
val set_title : string -> document -> unit

(** {4 Page Properties } *)

val page_width : document -> float
val page_height : document -> float

(** Top, right, bottom and left margin. *)
val margins : document -> float * float * float * float

val set_margins :
  left:float ->
  ?right:float -> top:float -> ?bottom:float -> document -> unit

(** Returns the current page number. *)
val page_no : document -> int


(** {4 Page Header and Footer} *)

(** Set the drawing function for the header. *)
val set_header_func : (unit -> unit) -> document -> unit

(** Set the drawing function for the footer. *)
val set_footer_func : (unit -> unit) -> document -> unit


(** {4 Positioning} *)

(** Return the current absolute {i x} position. *)
val x : document -> float

(** Return the current absolute {i y} positions. *)
val y : document -> float

(** Set the current absolute {i x} and {i y} position. *)
val set : ?x:float -> ?y:float -> document -> unit


(** {4 Text} *)

val font_style : document -> Font.style list
val font_size : document -> float
val font_family : document -> Font.family option
val set_font :
  ?family:Font.family ->
  ?style:Font.style list -> ?size:float -> document -> unit
val set_text_color : red:int -> ?green:int -> ?blue:int -> document -> unit
val text_color : document -> int * int * int
val fill_color : document -> int * int * int

val multi_cell :
  width:float ->
  line_height:float ->
  text:string ->
  ?border:border_part list ->
  ?padding:float -> ?align:align -> ?fill:bool -> document -> unit

val multi_cell_lines :
  width:float ->
  line_height:float ->
  text:string ->
  ?border:border_part list ->
  ?padding:float -> ?align:align -> ?fill:bool -> document -> string list

val cell :
  width:float ->
  ?height:float ->
  ?text:string ->
  ?border:border_part list ->
  ?padding:float ->
  ?ln:[ `Bottom | `Next_line | `Right ] ->
  ?align:align ->
  ?fill:bool -> ?link:string -> document -> unit

val write :
  height:float ->
  ?padding:float -> text:string -> ?link:string -> document -> unit

val text : x:float -> y:float -> text:string -> document -> unit

val newline : ?height:float -> document -> unit


(** {4 Graphics} *)

(** {6 Graphics state operators} *)

(** Save the current graphics state on the graphics state stack *)
val push_graphics_state : document -> unit

(** Restore the graphics state by removing the most recently saved state from
    the stack and making it the current state *)
val pop_graphics_state : document -> unit

(** Default value is 0.1 *)
val set_line_width : float -> document -> unit

val line_width : document -> float

val set_line_cap : line_cap_style -> document -> unit

val line_cap : document -> line_cap_style

val set_line_join : line_join_style -> document -> unit

val line_join : document -> line_join_style

val set_line_dash : int list -> ?phase:int -> document -> unit

val line_dash : document -> int list * int

val set_draw_color : red:int -> ?green:int -> ?blue:int -> document -> unit
val draw_color : document -> int * int * int
val set_fill_color : red:int -> ?green:int -> ?blue:int -> document -> unit
val fill_color : document -> int * int * int

(** {6 Drawing} *)

val line : x1:float -> y1:float -> x2:float -> y2:float -> document -> unit

(** Draw a rectangle.
  @param x Absolute abscissa of the upper-left corner.
  @param y Absolute ordinate of the upper-left corner.
  @param width Width of the rectangle.
  @param height Height of the rectangle.
  @param radius Border radius for rounded corners.
  @param style Style of rendering.
  *)
val rect :
  ?x:float ->
  ?y:float ->
  width:float ->
  height:float -> ?radius:float -> ?style:rect_style -> document -> unit

(** {6 Images} *)

(** Put an image in the document.
  @param name Filename with format [<id>.\{jpg|jpeg|png\}] (for example: {i myimage.jpg}).
  @param data Supported formats are JPEG (RGB color space) and PNG.
  @param x Abscissa of the upper-left corner.
  @param y Ordinate of the upper-left corner.
  @param image_width Width of the image in pixels.
  @param image_height Height of the image in pixels.
  @param width Width of the image in the page in user unit.
  @param height Height of the image in the page in user unit.
  *)
val image :
  name:string ->
  data:string ->
  x:float ->
  y:float ->
  image_width:int ->
  image_height:int ->
  ?width:float ->
  ?height:float -> ?format:string -> ?link:'a -> document -> unit


(** {4 Miscellaneous Functions} *)

(** Return the width of a string in user units computed with the current font. *)
val get_string_width : string -> document -> float


(** {4 Private} *)

val add_resource : (unit -> unit) -> document -> unit
val add_catalog : (unit -> unit) -> document -> unit
val new_obj : document -> unit
val current_object_number : document -> int
val print : document -> ('a, unit, string, unit) format4 -> 'a


(** {4 Hyperlinks } *)

(**
    @deprecated
  *)
val get_link : int -> document -> int * float

(**
    @deprecated
  *)
val add_link :
  x:'a -> y:'b -> width:'c -> height:'d -> link:'e -> unit -> unit


