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


type key =
    Courier
  | Courier_Oblique
  | Courier_Bold
  | Courier_BoldOblique
  | Helvetica
  | Helvetica_Oblique
  | Helvetica_Bold
  | Helvetica_BoldOblique
  | Times_Roman
  | Times_Bold
  | Times_Italic
  | Times_BoldItalic
  | ZapfDingbats
  | Symbol
  | CenturySchoolbook
  | CenturySchoolbook_Italic
  | CenturySchoolbook_Bold
  | CenturySchoolbook_BoldItalic
  | CMUSerif
  | CMUSerif_Bold
  | CMUSerif_Italic
  | CMUSerif_BoldItalic
  | CMUSansSerif
  | CMUSansSerif_Bold
  | CMUSansSerif_Oblique
  | CMUSansSerif_BoldOblique
  | CMUSerif_BoldNonextended
  | CMUSansSerif_DemiCondensed
type family = [
    | `Courier | `Helvetica | `Times | `Symbol | `ZapfDingbats | `CenturySchoolbook
    | `CMUSerif | `CMUSansSerif | `CMUSansSerif_DemiCondensed | `CMUSerif_BoldNonextended
]

type style = [ `Underline | `Italic | `Bold ]
type t = {
  fontType : [ `Additional of string | `Core | `TrueType | `Type1 ];
  fontName : string;
  fontFamily : family;
  fontWeight : int;
  flags : int option;
  italicAngle : float;
  ascent : int option;
  descent : int option;
  capHeight : int option;
  stemV : int option;
  missingWidth : int option;
  fontFile : (int * string) option;
  diff : string option;
  encoding : string option;
  fontBBox : int * int * int * int;
  underlinePosition : int;
  underlineThickness : int;
  charMetrics : char -> int;
}
val family_of_string : string -> family
val string_of_family : family -> string
val style_of_string : string -> style
val string_of_style : style -> string
val key_of_font : style list -> family -> key
val ascent : t -> int
val descent : t -> int
val baseline : t -> int
val line_gap : t -> int
val height : t -> int
val style : t -> style list
val fonts : (key, t) Hashtbl.t option ref
val find : key -> t
