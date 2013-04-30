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

(** Layout containers. *)


(** An horizontal container box.
    [hbox] is a container that organizes child boxes into a single row.
    @param x Abscissa of the upper-left corner.
    @param y Ordinate of the upper-left corner.
    @param width Width of the container.
    @param height Height of the container.
    @param spacing The amount of space between children.
    @param padding Extra space to put between the child and its neighbors.
    @param border Draw a red border around the container (for debug purposes).
  *)
class hbox :
  x:float ->
  y:float ->
  width:float ->
  height:float ->
  ?spacing:float ->
  ?padding:float ->
  ?border:bool ->
  PDF.t ->
  object

    method x : float
    method y : float
    method width : float
    method height : float
    method padding : float
    method spacing : float

    (** [get_homogeneous_child_width n] returns {i (width - 2 * padding - (n - 1) * spacing) / n } *)
    method get_homogeneous_child_width : int -> float

    (** Add a child box to the container and set the function to use to render inside it.

        The [x] and [y] arguments received by the rendering function are the absolute
        coordinates of the upper-left corner of the child box; argument [width] is
        the remaining width available for the child to use; argument [height] is
        the overall available height of the parent (without considering padding).
        The value returned by the function must be the actual width consumed by the
        child box.
    *)
    method add :
      (x:float -> y:float -> width:float -> height:float -> float) -> unit

    (** Draw the content of all boxes that have been added to the container. *)
    method pack : unit -> unit
  end

(** A vertical container box.
    [vbox] is a container that organizes child boxes into a single column.
    @param x Abscissa of the upper-left corner.
    @param y Ordinate of the upper-left corner.
    @param width Width of the container.
    @param height Height of the container.
    @param spacing The amount of space between children.
    @param padding Extra space to put between the child and its neighbors.
    @param border Draw a red border around the container (for debug purposes).
*)
class vbox :
  x:float ->
  y:float ->
  width:float ->
  height:float ->
  ?spacing:float ->
  ?padding:float ->
  ?border:bool ->
  PDF.t ->
  object

    method x : float
    method y : float
    method width : float
    method height : float
    method padding : float
    method spacing : float

    (** [get_homogeneous_child_height n] returns {i (height - 2 * padding - (n - 1) * spacing) / n } *)
    method get_homogeneous_child_height : int -> float

    (** Add a child box to the container and set the function to use to render inside it.

        The [x] and [y] arguments received by the rendering function are the absolute
        coordinates of the upper-left corner of the child box; argument [width] is
        the overall available width of the parent (without considering padding);
        argument [height] is the remaining height available for the child to use.
        The value returned by the function must be the actual height consumed by the
        child box.
    *)
    method add :
      (x:float -> y:float -> width:float -> height:float -> float) -> unit

    (** Draw the content of all boxes that have been added to the container. *)
    method pack : unit -> unit
  end

(** Pack child boxes in regular patterns.
    The [table] methods allow to arrange boxes in rows and columns, making it easy to
    align many boxes next to each other, horizontally and vertically.
    @param x Abscissa of the upper-left corner.
    @param y Ordinate of the upper-left corner.
    @param width Width of the table.
    @param height Height of the table.
    @param rows Number of rows.
    @param colums  Number of columns.
    @param spacing The amount of space between cells.
    @param padding The amount of space around a cell.
*)
class table :
  x:float ->
  y:float ->
  width:float ->
  height:float ->
  rows:int ->
  columns:int ->
  ?spacing:float ->
  ?padding:float ->
  PDF.t ->
  object

    method x : float
    method y : float

    (** [set row column f] sets [f] as the function to use to render the content
        of the cell at position [row] and [column].
        Arguments [x], [y], [width] and [height] provide absolute coordinates of
        the upper-left corner of the cell and its dimensions.
      *)
    method set :
      int ->
      int ->
      (x:float -> y:float -> width:float -> height:float -> unit) -> unit

    (** Render the content of all cells that have been set in the table. *)
    method pack : unit -> unit
  end
