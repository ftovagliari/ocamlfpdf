type t = {
  doc : Fpdf.t;
  x0 : float;
  y0 : float;
  mutable x : float;
  mutable y : float;
  mutable rows : int;
  mutable cols : int;
  mutable cells : ((int * int) * cell) list;
  colwidths : float array;
  border_width : thickness option;
  padding : float;
  mutable v_lines : (thickness option * int * int option * int) list;
  mutable h_lines : (thickness option * int * int option * int) list;
  mutable page_breaks : (int * (unit -> float option)) list;
  debug : bool;
}
and cell = {
  mutable cell_x : float;
  mutable cell_y : float;
  mutable width : float;
  mutable height : float;
  mutable cell_width : float;
  mutable cell_height : float;
  mutable rowspan : int;
  mutable colspan : int;
  callback :
    x:float -> y:float -> col_width:float -> row_height:float -> unit;
}
and thickness = [ `Medium | `Thick | `Thin | `Size of float ]
val line_disjoin : float
val size_of_thickness : [< `Medium | `Thick | `Thin | `Size of float ] -> float
val create :
  x:float ->
  y:float ->
  ?padding:float ->
  ?border_width:thickness ->
  width:float -> colwidths:float array -> ?debug:bool -> Fpdf.t -> t
val set :
  t ->
  int ->
  int ->
  width:float ->
  height:float ->
  ?rowspan:int ->
  ?colspan:int ->
  (x:float -> y:float -> col_width:float -> row_height:float -> unit) -> unit
val set_markup :
  t ->
  int ->
  int ->
  ?rowspan:int ->
  ?colspan:int ->
  ?xalign:float ->
  ?yalign:float -> ?padding:float * float * float * float ->
  ?line_spacing:float -> string -> unit
val find_cell : t -> int -> int -> cell option
val col_width : t -> int -> float
val row_height : t -> int -> float
val table_width : t -> float
val table_height : t -> float
val add_vertical_line :
  ?line_width:thickness ->
  rowstart:int -> ?rowstop:int -> col:int -> t -> unit
val add_horizontal_line :
  ?line_width:thickness ->
  colstart:int -> ?colstop:int -> row:int -> t -> unit
val add_page_break_before : int -> t -> (unit -> float option) -> unit
val line_intersection : 'a -> 'a -> 'b -> 'a -> 'b -> 'b -> 'a option
val build_matrix : t -> cell option array array
val iter_rows : cell option array array -> (int -> cell option array -> unit) -> unit
val pack : ?matrix:(cell option array array) -> t -> unit
