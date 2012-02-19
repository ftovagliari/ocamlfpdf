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

(** Print barcodes in Code39, EAN13 or Code128C format. *)

module Code39 :
  sig
    (**
      @param x Abscissa of the upper-left corner.
      @param y Ordinate of the upper-left corner.
      @param baseline The thickness of the bar.
      @param text Font size of the text under the barcode.
      @param height Height of the barcode (excluding the text below).
      *)
    val write :
      x:float ->
      y:float ->
      barcode:string ->
      ?baseline:float -> ?height:float -> ?text:float -> PDF.document -> unit

    val bar_char : (char * string) list
    val get_width : barcode:string -> float

  end

module EAN13 :
  sig
    exception Incorrect_check_digit of string
    val lpad : string -> int -> char -> string
    val codes : char -> char -> string
    val parities : char -> char array
    val test_check_digit : string -> bool
    val get_check_digit : string -> string
    val write :
      x:float ->
      y:float ->
      ?height:float ->
      ?width:float -> barcode:string -> ?upc_a:bool -> PDF.document -> unit
  end

module Code128C :
  sig
    val encode : int -> string
    val check_symbol : int list -> int
    val get_width : barcode:string -> int
    val write :
      x:float ->
      y:float ->
      ?height:float ->
      ?width:float ->
      ?set:[ `A | `B | `C ] ->
      ?text:float -> barcode:string -> PDF.document -> unit
  end

