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

(** Fonts Description. *)
type key
type family =
    [ `Courier | `Helvetica | `Symbol | `Times | `ZapfDingbats ]
type style = [ `Bold | `Italic | `Underline ]
val key_of_font :
  [> `Bold | `Italic ] list -> [> `Courier | `Helvetica | `Times ] -> key
val string_of_key : key -> string
val core_fonts : (key * string) list
val get_metric : key -> char -> int
val get_name : key -> string
val family_of_string : string -> family
val style_of_string : string -> style
val find :
  ?family:family ->
  style:[> `Bold | `Italic ] list -> (key * 'a) list -> 'a option