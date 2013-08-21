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

open Fpdf_types

(** Layout for printing tabular material. *)

(** How to realize the cell contents. You can declare a set of style properties
    or define a drawing function. *)
type cell_func =
  | Cell_properties of cell_properties
  | Cell_draw of float * (x:float -> y:float -> width:float -> height:float -> unit)
    (** The first element of the pair is the height that the function will use.
        The function receives as arguments [x] and [y] coordinates of the upper left
        corner of the cell and [width] and [height] available ([height] is the same
        value specified in the first element of the pair). *)

(** Properties for cell's style. *)
and cell_properties = {
  mutable prop_text       : string;
  mutable prop_align      : align;
  mutable prop_font_style : Font.style list;
  mutable prop_font_size  : float option;
  mutable prop_bg_color   : (int * int * int) option;
  mutable prop_fg_color   : (int * int * int) option;
  prop_image              : image option;
}
and image

(** Style properties for the column titles. *)
and column = {
  mutable col_width : float; (** Column width in percentage. *)
  mutable col_title : header_draw_func; (** Contents of the column header, specified either as text or as a drwing function. *)
}

(** Type for the user defined column identifier used to give a name to the table columns.
    For example, you may use a variant type. *)
and 'a column_id = 'a

(** Structure of the column header arranged hierarchically as a tree.

    - A node is a group of columns.
    - A leaf is a real table column with column identifier associated. *)
and 'a tree = [
  | `Node of 'a node
  | `Leaf of 'a column_id
]

(** Informations associated to a node. *)
and 'a node = {
  h_draw                : header_draw_func; (** Function to draw the header group content. *)
  h_children            : 'a tree list; (** Subtree. *)
}

and header_draw_func = [`Text of string | `Func of (x:float -> y:float -> width:float -> float)]

and thickness = [`Thin | `Medium | `Thick ]

(** Table layout with automatic page break.
  @param x Absolute abscissa of the upper-left corner.
  @param y Absolute ordinate of the upper-left corner.
  @param width Width of the table
  @param page_height Available height in the page body.
  @param columns Columns description. The first element of the pairs is a user defined column identifier.
  @param rows The list of rows.
  @param header_layout Layout definition of the table header.
  @param grid_lines Whether grid lines should be drawn in the table body.
  @param border Which parts of the table border should be drawn.
  @param border_width Width of the table border.
  @param page_header_height Height of the page header.
  @param page_break_func Function called when a page break occurs.
  @param cellpadding Extra space to put around cell contents.
  @param line_spacing Line spacing (default is [1.0]).
  @param cell_func Function applied to every single cell of the table to draw its content or set style properties.
         The [cell_func] returned is applied to the cell identified by [index] and [col],
         where [index] is the general row index starting from zero and
         [(row column_id)] returns the content of the cell at index [index] and column [column_id].
  @param use_markup Whether the cell's content should be interpreted as markup (i.e. printed with [Fpdf_markup]).
  @param caption Caption of the table.
  @param doc A [PDF] document.
*)
val print :
  x:float ->
  y:float ->
  width:float ->
  page_height:float ->
  columns:('a column_id * column) list ->
  rows:string option array list ->
  ?header_layout:('a tree list) ->
  ?grid_lines : [`None | `Vertical | `Horizontal | `Both] ->
  ?border : border_part list ->
  ?border_width:thickness ->
  ?page_header_height:float ->
  ?page_break_func:(unit -> unit) ->
  ?cellpadding:float ->
  ?rowspacing:float ->
  ?line_spacing:float ->
  ?cell_func:(index:int ->
              row:('a column_id -> string option) -> col:('a column_id) -> cell_func) ->
  ?use_markup:bool ->
  ?caption:string ->
  Fpdf.t -> unit
