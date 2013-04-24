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

(** Error reporting. *)

(** Error codes for this library. *)
type t =
    Bad_image_format of string
  | Unsupported_image_format
  | Unsupported_16_bit_depth_image
  | Unknown_color_type
  | Missing_palette
  | Invalid_markup

(** Exception raised by functions in this library to report error conditions. *)
exception Error of t * string

(** Get a full error message. *)
val message : t -> string
