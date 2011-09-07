(*

  OCaml-FPDF
  Copyright (C) 2010 Francesco Tovagliari

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


type key = Times | Times_B | Times_I | Times_BI | Helvetica | Helvetica_B | Helvetica_I | Helvetica_BI | Courier | Courier_I | Courier_BI | Courier_B

type family = [`ZapfDingbats | `Symbol | `Arial | `Helvetica | `Courier | `Times]
(* Alis: Arial = Helvetica; Symbol = ZapfDingbats *)

type style = [`Underline | `Italic | `Bold]


let key_of_font style =
  let bold = List.mem `Bold style in
  let italic = List.mem `Italic style in
  function
    | `Times when bold && italic -> Times_BI
    | `Times when bold -> Times_B
    | `Times when italic -> Times_I
    | `Times -> Times
    | `Helvetica when bold && italic -> Helvetica_BI
    | `Helvetica when bold -> Helvetica_B
    | `Helvetica when italic -> Helvetica_I
    | `Helvetica -> Helvetica
    | `Courier when bold && italic -> Courier_BI
    | `Courier when bold -> Courier_B
    | `Courier when italic -> Courier_I
    | `Courier -> Courier
    | _ -> raise Not_found

let string_of_key = function
  | Times -> "Times"
  | Times_B -> "Times Bold"
  | Times_I -> "Times Italic"
  | Times_BI -> "Times Bold Italic"
  | Helvetica -> "Helvetica"
  | Helvetica_B -> "Helvetica Bold"
  | Helvetica_I -> "Helvetica Italic"
  | Helvetica_BI -> "Helvetica Bold Italic"
  | Courier -> "Courier"
  | Courier_I -> "Courier Italic"
  | Courier_B -> "Courier Bold"
  | Courier_BI -> "Courierb Bold Italic"

(** Nomi dei font standard *)
let core_fonts = [
(* ("helvetica", "Helvetica"); ("helveticaB", "Helvetica-Bold");
  ("helveticaI", "Helvetica-Oblique"); ("helveticaBI", "Helvetica-BoldOblique");*)
  (Courier, "Courier");
  (Courier_I, "Courier-Oblique");
  (Courier_B, "Courier-Bold");
  (Courier_BI, "Courier-BoldOblique");
  (Times, "Times-Roman");
  (Times_B, "Times-Bold");
  (Times_I, "Times-Italic");
  (Times_BI, "Times-BoldItalic");
  (Helvetica, "Helvetica");
  (Helvetica_B, "Helvetica-Bold");
  (Helvetica_I, "Helvetica-Oblique");
  (Helvetica_BI, "Helvetica-BoldOblique");
(*  ("symbol", "Symbol"); ("zapfdingbats", "ZapfDingbats")*)
]

let get_metric = function
  | Times -> Times.normal
  | Times_B -> Times.bold
  | Times_I -> Times.italic
  | Times_BI -> Times.bold_italic
  | Helvetica -> Helvetica.normal
  | Helvetica_B -> Helvetica.bold
  | Helvetica_I -> Helvetica.italic
  | Helvetica_BI -> Helvetica.bold_italic
  | Courier -> Courier.normal
  | Courier_I -> Courier.italic
  | Courier_B -> Courier.bold
  | Courier_BI -> Courier.bold_italic

let get_name key = List.assoc key core_fonts

let family_of_string name =
  match String.lowercase name with
  | "times" -> `Times
  | "helvetica" | "arial" -> `Helvetica
  | "courier" -> `Courier
  | "zapfdingbats" -> `ZapfDingbats
  | "symbol" -> `Symbol
  | _ -> Printf.kprintf failwith "family_of_string (%s)" name

let style_of_string name =
  match String.lowercase name with
  | "underline" -> `Underline
  | "italic" -> `Italic
  | "bold" -> `Bold
  | _ -> Printf.kprintf failwith "style_of_string (%s)" name















