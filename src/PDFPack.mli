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

(** Horizontal and vetical box layout. *)


(** A horizontal container box.
    @param x Abscissa of the upper-left corner.
    @param y ordinate of the upper-left corner.
    @param width Total width of the container.
    @param height Total height of the container.
    @param spacing The amount of space between children.
    @param padding Extra space to put between the child and its neighbors.
    @param border Draw a red border around the container (for debugging purposes).
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

    (** Add a child box drawn with the given function.
      @param x Absolute abscissa of the upper-left corner of the child box.
      @param y Absolute ordinate of the upper-left corner of the child box.
      @param width Total available width of the container.
      @param height Total available height of the container.
      @return The actual width of the child box.
    *)
    method add :
      (x:float -> y:float -> width:float -> height:float -> float) -> unit

    (** Print all boxes that have been added to the container. *)
    method pack : unit -> unit
  end

(** A vertical container box.
    @param x Abscissa of the upper-left corner.
    @param y Ordinate of the upper-left corner.
    @param width Total width of the container.
    @param height Total height of the container.
    @param spacing The amount of space between children.
    @param padding Extra space to put between the child and its neighbors.
    @param border Draw a red border around the container (for debugging purposes).
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

    (** Add a child box drawn with the given function.
      @param x Absolute abscissaof the upper-left corner of the child box.
      @param y Absolute ordinateof the upper-left corner of the child box.
      @param width Total available width of the container.
      @param height Total available height of the container.
      @return The actual height of the child box.
    *)
    method add :
      (x:float -> y:float -> width:float -> height:float -> float) -> unit

    method dim : unit -> float

    (** Print all boxes that have been added to the container. *)
    method pack : unit -> unit
  end

(** Pack child boxes in regular patterns.
    @param x Abscissa of the upper-left corner.
    @param y Ordinate of the upper-left corner.
    @param width Total width of the container.
    @param height Total height of the container.
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
    method pack : unit -> unit
    method set :
      int ->
      int ->
      (x:float -> y:float -> width:float -> height:float -> unit) -> unit
  end
