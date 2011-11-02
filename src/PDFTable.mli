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

(** Table Layout *)
type cell_properties = {
  mutable prop_text : string;
  mutable prop_align : PDF.align;
  mutable prop_font_style : Font.style list;
  mutable prop_font_size : float option;
  mutable prop_bg_color : (int * int * int) option;
  mutable prop_fg_color : (int * int * int) option;
  prop_image : image option;
}
and image
and column = { mutable col_width : float; mutable col_title : string; }

type 'a column_id = 'a

(** Table layout with automatic page break.
  @param x Abscissa of the upper-left corner relative to the page's left margin.
  @param y Ordinate of the upper-left corner relative to the page's top margin.
  @param width Total width of the table
  @param page_height Available height of the page body.
  @param line_height Height of the line of text.
  @param columns Columns description. The first element of the pairs is a column identifier,
         for example it may be a user defined variant tag.
  @param rows The list of rows.
  @param caption Caption of the table.
  @param cell_func Function applied to every single cell of the table to set style properties.
         The [cell_properties] returned are applied to the cell identified by [index] and [col],
         where [index] is the general row index starting from zero.
         [(row column_id)] returns the content of the cell at index [index] and column [column_id].
  @param doc A [PDF] document.
*)
val print :
  x:float ->
  y:float ->
  width:float ->
  page_height:float ->
  line_height:float ->
  columns:('a column_id * column) list ->
  rows:string option array list ->
  ?caption:string ->
  ?cell_func:(index:int ->
              row:('a column_id -> string option) -> col:('a column_id) -> cell_properties) ->
  PDF.document -> unit
