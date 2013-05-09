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


open Font_metrics

type key =
    | Courier | Courier_Oblique | Courier_Bold | Courier_BoldOblique
    | Helvetica | Helvetica_Oblique | Helvetica_Bold | Helvetica_BoldOblique
    | Times_Roman | Times_Bold | Times_Italic | Times_BoldItalic
    | ZapfDingbats
    | Symbol

type family = [ `Courier | `Helvetica | `Times | `Symbol | `ZapfDingbats ]

type style = [ `Underline | `Italic | `Bold ]

(** family_of_string *)
let family_of_string name =
  match String.lowercase name with
  | "times" -> `Times
  | "helvetica" -> `Helvetica
  | "courier" -> `Courier
  | "zapfdingbats" -> `ZapfDingbats
  | "symbol" -> `Symbol
  | _ -> Printf.kprintf failwith "family_of_string (%s)" name

(** string_of_family *)
let string_of_family = function
  | `Times -> "times"
  | `Helvetica -> "helvetica"
  | `Courier -> "courier"
  | `ZapfDingbats -> "zapfdingbats"
  | `Symbol -> "symbol"

(** style_of_string *)
let style_of_string name =
  match String.lowercase name with
  | "underline" -> `Underline
  | "italic" -> `Italic
  | "bold" -> `Bold
  | _ -> Printf.kprintf failwith "style_of_string (%s)" name

(** string_of_style *)
let string_of_style = function
  | `Italic -> "italic"
  | `Bold -> "bold"
  | `Underline -> "underline"

(** string_of_key *)
let string_of_key = function
  | Courier -> "Courier"
  | Courier_Oblique -> "Courier-Oblique"
  | Courier_Bold -> "Courier-Bold"
  | Courier_BoldOblique -> "Courier-BoldOblique"
  | Helvetica -> "Helvetica"
  | Helvetica_Oblique -> "Helvetica-Oblique"
  | Helvetica_Bold -> "Helvetica-Bold"
  | Helvetica_BoldOblique -> "Helvetica-BoldOblique"
  | Times_Roman -> "Times-Roman"
  | Times_Bold -> "Times-Bold"
  | Times_Italic -> "Times-Italic"
  | Times_BoldItalic -> "Times-BoldItalic"
  | ZapfDingbats -> "ZapfDingbats"
  | Symbol -> "Symbol"

(** key_of_font *)
let key_of_font style =
  let bold = List.mem `Bold style in
  let italic = List.mem `Italic style in
  function
    | `Times when bold && italic -> Times_BoldItalic
    | `Times when bold -> Times_Bold
    | `Times when italic -> Times_Italic
    | `Times -> Times_Roman
    | `Helvetica when bold && italic -> Helvetica_BoldOblique
    | `Helvetica when bold -> Helvetica_Bold
    | `Helvetica when italic -> Helvetica_Oblique
    | `Helvetica -> Helvetica
    | `Courier when bold && italic -> Courier_BoldOblique
    | `Courier when bold -> Courier_Bold
    | `Courier when italic -> Courier_Oblique
    | `Courier -> Courier
    | `Symbol -> Symbol
    | `ZapfDingbats -> ZapfDingbats
    | _ -> raise Not_found

(** fonts *)
let fonts : (key, Font_metrics.t) Hashtbl.t = Hashtbl.create 17

let _ =
  Hashtbl.add fonts Courier Courier.descriptor;
  Hashtbl.add fonts Courier_Oblique Courier_Oblique.descriptor;
  Hashtbl.add fonts Courier_Bold Courier_Bold.descriptor;
  Hashtbl.add fonts Courier_BoldOblique Courier_BoldOblique.descriptor;

  Hashtbl.add fonts Helvetica Helvetica.descriptor;
  Hashtbl.add fonts Helvetica_Oblique Helvetica_Oblique.descriptor;
  Hashtbl.add fonts Helvetica_Bold Helvetica_Bold.descriptor;
  Hashtbl.add fonts Helvetica_BoldOblique Helvetica_BoldOblique.descriptor;

  Hashtbl.add fonts Times_Roman Times_Roman.descriptor;
  Hashtbl.add fonts Times_Bold Times_Bold.descriptor;
  Hashtbl.add fonts Times_Italic Times_Italic.descriptor;
  Hashtbl.add fonts Times_BoldItalic Times_BoldItalic.descriptor;

  Hashtbl.add fonts ZapfDingbats ZapfDingbats.descriptor;

  Hashtbl.add fonts Symbol Symbol.descriptor;
;;

(** find *)
let find = Hashtbl.find fonts

(** family *)
let family = function
  | Courier | Courier_Oblique | Courier_Bold | Courier_BoldOblique -> `Courier
  | Helvetica | Helvetica_Oblique | Helvetica_Bold | Helvetica_BoldOblique -> `Helvetica
  | Times_Roman | Times_Bold | Times_Italic | Times_BoldItalic -> `Times
  | ZapfDingbats -> `ZapfDingbats
  | Symbol -> `Symbol

(** style *)
let style = function
  | Courier -> []
  | Courier_Oblique -> [`Italic]
  | Courier_Bold -> [`Bold]
  | Courier_BoldOblique -> [`Bold; `Italic]
  | Helvetica -> []
  | Helvetica_Oblique -> [`Italic]
  | Helvetica_Bold -> [`Bold]
  | Helvetica_BoldOblique -> [`Bold; `Italic]
  | Times_Roman -> []
  | Times_Bold -> [`Bold]
  | Times_Italic -> [`Italic]
  | Times_BoldItalic -> [`Bold; `Italic]
  | ZapfDingbats -> []
  | Symbol -> []

(** line_gap *)
let line_gap font =
  let _, _, _, ruy = font.fontBBox in
  (float (1000 - ruy)) /. 1000.

(** descent *)
let descent font =
  let _, lby, _, _ = font.fontBBox in (* lby < 0 *)
  -.((float lby) /. 1000.)


