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

open FPDFTypes

(** PDF document creation and operators. *)


(** The PDF document type. *)
type t = FPDFDocument.t

(** {6 PDF Document} *)

(**
  @param m_unit Default is [Mm].
  @param orientation Default is [Portrait].
  @param format Default is [A4].
  @param outchan The output channel where to send the document data.
  *)
val create :
  ?orientation:orientation ->
  ?m_unit:m_unit -> ?format:format -> outchan:out_channel -> unit -> t

(** Append a new page to the document.
    @param orientation Default value is the document orientation passed to [create].
  *)
val add_page : ?orientation:orientation -> t -> unit

(** Close the PDF document.
    After this point, no further operation is possible on the document. *)
val close_document : t -> unit

val scale : t -> float
val page_count : t -> int

val set_double_sided : bool -> t -> unit
val double_sided : t -> bool
val set_auto_page_break : ?margin:float -> bool -> t -> unit
val set_display_mode : ?layout:layout -> zoom -> t -> unit

(** Set a sequence of actions to be performed when the document is opened. *)
val set_open_actions : action list -> t -> unit

(** {7 Document information } *)

val set_author : string -> t -> unit
val set_title : string -> t -> unit
val set_subject : string -> t -> unit
val set_creator : string -> t -> unit

(** A date is an ASCII string of the form: [(D:YYYYMMDDHHmmSSOHH'mm')].
    See {i PDF Reference version 1.7, section 3.8.3} for more informations.
  *)
val set_creation_date : string -> t -> unit
val set_keywords : string list -> t -> unit

val author : t -> string
val title : t -> string
val subject : t -> string
val creator : t -> string
val creation_date : t -> string
val keywords : t -> string list


(** {6 Page Properties } *)

val page_width : t -> float
val page_height : t -> float

(** Top, right, bottom and left margin. *)
val margins : t -> float * float * float * float

val set_margins :
  left:float ->
  ?right:float -> top:float -> ?bottom:float -> t -> unit

(** Returns the current page number. *)
val page_num : t -> int


(** {6 Page Header and Footer} *)

(** Set the drawing function for the header. *)
val set_header_func : (unit -> unit) -> t -> unit

(** Set the drawing function for the footer. *)
val set_footer_func : (unit -> unit) -> t -> unit


(** {6 Positioning} *)

(** Return the current absolute {i x} position. *)
val x : t -> float

(** Return the current absolute {i y} positions. *)
val y : t -> float

(** Set the current absolute {i x} and {i y} position. *)
val set : ?x:float -> ?y:float -> t -> unit


(** {6 Text} *)

val font_style : t -> Font.style list
val font_size : t -> float
val font_scale : t -> int option
val font_char_space : t -> float option
val font_family : t -> Font.family option
val set_font :
  ?family:Font.family ->
  ?style:Font.style list ->
  ?size:float -> ?scale:int -> ?char_space:float -> t -> unit
val set_text_color : red:int -> ?green:int -> ?blue:int -> t -> unit
val text_color : t -> int * int * int
val fill_color : t -> int * int * int

val multi_cell :
  width:float ->
  line_height:float ->
  text:string ->
  ?border:border_part list ->
  ?padding:float -> ?align:align -> ?fill:bool -> t -> unit

val multi_cell_lines :
  width:float ->
  line_height:float ->
  text:string ->
  ?border:border_part list ->
  ?padding:float -> ?align:align -> ?fill:bool -> t -> string list

val cell :
  ?width:float ->
  ?height:float ->
  ?text:string ->
  ?font_family:Font.family ->
  ?font_style:Font.style list ->
  ?font_size:float ->
  ?font_scale:int ->
  ?char_space:float ->
  ?rise:float ->
  ?border:border_part list ->
  ?padding:float ->
  ?ln:[ `Bottom | `Next_line | `Right ] ->
  ?align:align ->
  ?fill:bool -> ?link:string -> t -> unit

val write :
  height:float ->
  ?padding:float -> text:string -> ?link:string -> t -> unit

val text : x:float -> y:float -> text:string -> t -> unit

val newline : ?height:float -> t -> unit

(** Return the width of a string in unscaled text space units computed with the given font. *)
val get_text_width : Font.t -> float -> string -> float

(** {6 Graphics} *)

(** {7 Graphics state operators} *)

(** Save the current graphics state on the graphics state stack *)
val push_graphics_state : t -> unit

(** Restore the graphics state by removing the most recently saved state from
    the stack and making it the current state *)
val pop_graphics_state : t -> unit

(** Default value is 0.1 *)
val set_line_width : float -> t -> unit

val line_width : t -> float

val set_line_cap : line_cap_style -> t -> unit

val line_cap : t -> line_cap_style

val set_line_join : line_join_style -> t -> unit

val line_join : t -> line_join_style

val set_line_dash : int list -> ?phase:int -> t -> unit

val line_dash : t -> int list * int

val set_draw_color : red:int -> ?green:int -> ?blue:int -> t -> unit
val draw_color : t -> int * int * int
val set_fill_color : red:int -> ?green:int -> ?blue:int -> t -> unit
val fill_color : t -> int * int * int

(** {7 Drawing} *)

val line : x1:float -> y1:float -> x2:float -> y2:float -> t -> unit

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
  height:float -> ?radius:float -> ?style:rect_style -> t -> unit

(** {7 Images} *)

(** Put an image in the document. Supported formats are JPEG (RGB color space only) and PNG.
  @param name Unique identifier of the image. If a name is used several times,
    only the data of the first copy is embedded in the PDF.
  @param data Image data.
  @param x Abscissa of the upper-left corner.
  @param y Ordinate of the upper-left corner.
  @param width Width of the image in the page in user unit.
  @param height Height of the image in the page in user unit.
  @raise Error(Unsupported_image_format) [ ]
  @raise Error(Missing_palette) [ ]
  @raise Error(Unsupported_16_bit_depth_image) [ ]
  *)
val image :
  name:string ->
  data:string ->
  x:float ->
  y:float ->
  ?width:float ->
  ?height:float -> ?link:'a -> t -> unit


(** Return width and height of an image data. Supported formats are JPEG and PNG.
  @raise Error(Unsupported_image_format) [ ]
  *)
val get_image_dimensions : string -> int * int



