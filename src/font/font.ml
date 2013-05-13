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


type key =
    | Courier | Courier_Oblique | Courier_Bold | Courier_BoldOblique
    | Helvetica | Helvetica_Oblique | Helvetica_Bold | Helvetica_BoldOblique
    | Times_Roman | Times_Bold | Times_Italic | Times_BoldItalic
    | ZapfDingbats
    | Symbol
    | CenturySchoolbook | CenturySchoolbook_Italic | CenturySchoolbook_Bold | CenturySchoolbook_BoldItalic
    | LMRoman10

type family = [ `Courier | `Helvetica | `Times | `Symbol | `ZapfDingbats | `CenturySchoolbook | `LMRoman10 ]

type style = [ `Underline | `Italic | `Bold ]

type t = {
  fontType           : [ `Core | `Type1 | `TrueType | `Additional of string ];
  fontName           : string;
  fontFamily         : family;
  fontWeight         : int;
  flags              : int option;
  italicAngle        : float;
  ascent             : int option;
  descent            : int option;
  capHeight          : int option;
  stemV              : int option;
  missingWidth       : int option;
  fontFile           : (int * string) option; (* original size and gz-compressed data of the font file *)
  diff               : string option;
  encoding           : string option;
  fontBBox           : (int * int * int * int); (* lowerLeftX, lowerLeftY, upperRightX, upperRightY *)
  underlinePosition  : int;
  underlineThickness : int;
  charMetrics        : char -> int;
}

(** family_of_string *)
let family_of_string = function
  | "Times" -> `Times
  | "Helvetica" -> `Helvetica
  | "Courier" -> `Courier
  | "ZapfDingbats" -> `ZapfDingbats
  | "Symbol" -> `Symbol
  | "CenturySchoolbook" -> `CenturySchoolbook
  | "LMRoman10" -> `LMRoman10
  | name -> Printf.kprintf failwith "family_of_string (%s)" name

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
    | `CenturySchoolbook when bold && italic -> CenturySchoolbook_BoldItalic
    | `CenturySchoolbook when italic -> CenturySchoolbook_Italic
    | `CenturySchoolbook when bold -> CenturySchoolbook_Bold
    | `CenturySchoolbook -> CenturySchoolbook
    | `LMRoman10 -> LMRoman10
    | _ -> raise Not_found

(** ascent *)
let ascent font =
  match font.ascent with
      | Some x -> x
      | _ -> let _, _, _, ruy = font.fontBBox in ruy

(** descent *)
let descent font =
  let _, lby, _, _ = font.fontBBox in (* lby < 0 *)
  -lby

(** baseline *)
let baseline font =
  let _, _, _, ruy = font.fontBBox in
  max 1000 ruy

(** line_gap *)
let line_gap font =
  let _, _, _, ruy = font.fontBBox in
  let ascent =
    match font.ascent with
      | Some x -> x
      | _ -> ruy
  in
  max 1000 ruy - ascent   (*baseline font - ascent font*)

(** height *)
let height font =
  let _, lby, _, ruy = font.fontBBox in (* lby < 0 *)
  max ruy 1000 - lby

(** style *)
let style font =
  (if font.fontWeight > 400 then [`Bold] else []) @ (if font.italicAngle <> 0. then [`Italic] else [])

(** find *)
let fonts : (key, t) Hashtbl.t option ref = ref None

let find key = match !fonts with
  | Some fonts -> Hashtbl.find fonts key
  | _ -> failwith "Font.find"





