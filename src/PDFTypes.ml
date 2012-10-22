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

type format = [ `A3 | `A4 | `A5 | `Letter | `Legal | `Custom_format of float * float ]

type orientation = [`Portrait | `Landscape]

type m_unit = [ `Mm | `Pt | `Cm | `In ]

type layout = [ `Single | `Continuous | `Two | `Default_layout ]

type zoom = [ `Fullpage | `Fullwidth | `Real | `Default_zoom | `Custom_zoom of float ]

type line_cap_style = [`Butt | `Round | `Square]

type line_join_style = [`Miter | `Round | `Bevel]

type rect_style = [ `Fill | `Both | `Outline ]

type border_part = [ `All | `L | `B | `R | `T ]

type align = [ `Left | `Justified | `Center | `Right ]

type destination = {
  dest_page    : int; (** Destination page number. *)
  dest_display : [
    | `XYZ of float option * float option * float
    | `Fit
    | `FitH of float option
    | `FitV of float option
  ];  (** Display mode *)
}

type action = [
  | `ResetForm
  | `GoTo of destination
]






