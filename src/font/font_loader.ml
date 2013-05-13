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


open Font

(** fonts *)
let fonts : (key, Font.t) Hashtbl.t = Hashtbl.create 17

let load_fonts () =
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
  (*  *)
  Hashtbl.add fonts CenturySchoolbook CenturySchoolbook.descriptor;
  Hashtbl.add fonts CenturySchoolbook_Italic CenturySchoolbook_Italic.descriptor;
  Hashtbl.add fonts CenturySchoolbook_Bold CenturySchoolbook_Bold.descriptor;
  Hashtbl.add fonts CenturySchoolbook_BoldItalic CenturySchoolbook_BoldItalic.descriptor;
  (*  *)
  Hashtbl.add fonts CMUSerif CMUSerif.descriptor;
  Hashtbl.add fonts CMUSansSerif CMUSansSerif.descriptor;
  Hashtbl.add fonts CMUSansSerif_Bold CMUSansSerif_Bold.descriptor;
  Hashtbl.add fonts CMUSansSerif_Oblique CMUSansSerif_Oblique.descriptor;
  Hashtbl.add fonts CMUSansSerif_BoldOblique CMUSansSerif_BoldOblique.descriptor;
  (*  *)
  Font.fonts := Some fonts
;;

