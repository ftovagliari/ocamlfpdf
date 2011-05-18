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
  @param y Ordinate of the upper-left corner relative to top page's top margin.
  @param width Total width of the table
  @param page_height Available height of the page body.
  @param line_height Height of the line of text.
  @param columns Columns description. The first element of the pairs is a column identifier,
         for example it may be a user defined variant tag.
  @param rows The list of rows.
  @param caption Caption of the table.
  @param cell_func Function applied to every single cell of the table to get style properties.
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
