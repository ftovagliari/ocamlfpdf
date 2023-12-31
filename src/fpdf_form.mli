(*

  OCaml-FPDF
  Copyright (C) 2011 Francesco Tovagliari

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


(** Interactive Forms.

    A collection of fields for gathering information interactively from the user. *)


(** The abstract type for forms. *)
type t

(** The abstract type for fields. *)
type field

(** Javascript actions that can be triggered by specific events. *)
type action = [
  | `Keystroke of string
  | `Value_changed  of string
  | `Calculate of string
]

(** [get doc] returns the form object associated with the document [doc]. *)
val get : Fpdf.t -> t

(** The list of fields currently present in the form. *)
val fields : t -> field list

(**
  @param x Absolute abscissa of the upper-left corner.
  @param y Absolute ordinate of the upper-left corner.
  @param width Width of the text field.
  @param height Height of the text field.
  @param name The partial field name.
  @param alt_name An alternate field name to be used in place of the actual field name wherever the field must be identified in the user interface (such as in error or status messages referring to the field).
  @param border Optional border around the text field.
  @param bgcolor Background color.
  @param fgcolor Text color.
  @param bgcolor_ap Background color .
  @param fgcolor_ap Text color.
  @param font_family Font family.
  @param font_size Font size. A zero value means that the font size is to be {i autosized} according to [height].
  @param font_style Font size.
  @param font_scale Font scale.
  @param maxlength The maximum length of the field's text, in characters.
  @param comb See {i PDF Reference version 1.7, table 8.77}
  @param readonly Whether the user may change the value of the field.
  @param hidden If set, do not display or print the field or allow it to interact with the user.
  @param justification Justification;
  @param value Field value
  @param value_ap Field value
  @param default_value Default value.
  @param actions Form field's additional-actions
  @param parent Parent element.
  @return An object representing the text field.
*)
val add_text_field :
  x:float ->
  y:float ->
  width:float ->
  height:float ->
  name:string ->
  ?alt_name:string ->
  ?border:[ `Dashed | `Solid | `Underline ] * string ->
  ?bgcolor:string ->
  ?fgcolor:string ->
  ?bgcolor_ap:string ->
  ?fgcolor_ap:string ->
  ?font_family:Font.family option ->
  ?font_size:float ->
  ?font_style:Font.style list ->
  ?font_scale:int ->
  ?maxlength:int ->
  ?comb:int ->
  ?readonly:bool ->
  ?hidden:bool ->
  ?justification:[ `Center | `Left | `Right ] ->
  ?value:string ->
  ?value_ap:string ->
  ?default_value:string ->
  ?actions:action list ->
  ?parent:field -> t -> field

(** [set_calculation_order fields form] Sets the calculation order for fileds with calculation action. *)
val set_calculation_order : field list -> t -> unit

(** [field_name field] returns the field's name. *)
val field_name : field -> string
