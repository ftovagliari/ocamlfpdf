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

(** Layout containers. *)


(** A horizontal container box.
    [hbox] is a container that organizes child boxes into a single row.
    @param x Abscissa of the upper-left corner.
    @param y Ordinate of the upper-left corner.
    @param width Width of the container.
    @param height Height of the container.
    @param spacing The amount of space between children.
    @param padding Extra space to put between the child and its neighbors.
    @param border Draw a red border around the container (to be used for debugging purposes).
  *)
class hbox :
  x:float ->
  y:float ->
  width:float ->
  height:float ->
  ?spacing:float ->
  ?padding:float ->
  ?border:bool ->
  PDF.document ->
  object

    (** Add a child box to the container and set the function to use to render inside it.

        The rendering function receives arguments [x] and [y], which are the absolute
        coordinates of the upper-left corner of the child box, and arguments
        [width] and [height] providing the overall available space of the parent,
        which can be used for rendering. The value returned by the function must be
        the actual width of the child box.
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
    @param border Draw a red border around the container (to be used for debugging purposes).
*)
class vbox :
  x:float ->
  y:float ->
  width:float ->
  height:float ->
  ?spacing:float ->
  ?padding:float ->
  ?border:bool ->
  PDF.document ->
  object

    (** Add a child box to the container and set the function to use to render inside it.

        The rendering function receives arguments [x] and [y], which are the absolute
        coordinates of the upper-left corner of the child box, and arguments
        [width] and [height] providing the overall available space of the parent,
        which can be used for rendering. The value returned by the function must be
        the actual height of the child box.
    *)
    method add :
      (x:float -> y:float -> width:float -> height:float -> float) -> unit

    (** @return The width of the container. *)
    method dim : unit -> float

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
  PDF.document ->
  object

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
