(*

  OCaml-FPDF
  Copyright (C) 2010-2013 Francesco Tovagliari

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

type t =
  | Bad_image_format of string
  | Unsupported_image_format
  | Unsupported_16_bit_depth_image
  | Unknown_color_type
  | Missing_palette
  | Invalid_markup
  | No_such_column
  | Invalid_colwidths

exception Error of t * string

let message = function
  | Bad_image_format fmt -> "Image format is not " ^ fmt
  | Unsupported_image_format -> "Image format is not supported"
  | Unsupported_16_bit_depth_image -> "16-bit depth image not supported"
  | Unknown_color_type -> "Unknown color type"
  | Missing_palette -> "Missing palette"
  | Invalid_markup -> "Invalid markup"
  | No_such_column -> "No such column"
  | Invalid_colwidths -> "Invalid column widths"

let error ex msg = raise (Error (ex, msg))

let handle_error f x =
  try Some (f x)
  with Error (err, msg) -> begin
    Printf.eprintf "%s (%s)\n" (message err) msg;
    Printexc.print_backtrace stderr;
    None
  end | ex -> raise ex
