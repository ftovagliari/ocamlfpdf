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

open PDFTypes
open PDFDocument
open PDFUtil

let int_of_linecap = function `Butt -> 0 | `Round -> 1 | `Square -> 2
let linecap_of_int = function 0 -> `Butt | 1 -> `Round | 2 -> `Square | _ -> assert false

let int_of_linejoin = function `Miter -> 0 | `Round -> 1 | `Bevel -> 2
let linejoin_of_int = function 0 -> `Miter | 1 -> `Round | 2 -> `Bevel | _ -> assert false

(** line_width *)
let set_line_width width doc =
  doc.line_width <- width;
  if doc.page >= 0 then (print_buffer doc "%.2f w\n" (width *. doc.k))

let line_width doc = doc.line_width

(** line_cap *)
let set_line_cap style doc =
  doc.line_cap <- style;
  if doc.page >= 0 then (print_buffer doc "%d J\n" (int_of_linecap style));;

let line_cap doc = doc.line_cap

(** line_join *)
let set_line_join style doc =
  doc.line_join <- style;
  if doc.page >= 0 then (print_buffer doc "%d j\n" (int_of_linejoin style));;

let line_join doc = doc.line_join

(** Line dash pattern *)
let set_line_dash pattern ?(phase=0) doc =
  doc.line_dash <- (pattern, phase);
  if doc.page >= 0 then (print_buffer doc "[%s] %d d\n"
    (String.concat " " (List.map string_of_int pattern)) phase);;

let line_dash doc = doc.line_dash

(** Graphics state stack *)
let push doc = if doc.page >= 0 then (print_buffer doc "q\n")

let pop doc = if doc.page >= 0 then (print_buffer doc "Q\n")
