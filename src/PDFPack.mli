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

(** Horizontal and vetical box layout *)


(** Horiontal boxes
    @param x Abscissa of the upper-left corner.
    @param y Ordinate of the upper-left corner.
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
    method add :
      (x:float -> y:float -> width:float -> height:float -> float) -> unit
    method pack : unit -> unit
  end

(** Vertical boxes
    @param x Abscissa of the upper-left corner.
    @param y Ordinate of the upper-left corner.
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
    method add :
      (x:float -> y:float -> width:float -> height:float -> float) -> unit
    method dim : unit -> float
    method pack : unit -> unit
  end

(** Table layout *)
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
