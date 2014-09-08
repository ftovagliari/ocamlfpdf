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


class editor :
  buffer:GText.buffer ->
  ?width:int ->
  ?height:int ->
  ?border_width:int ->
  ?shadow_type:Gtk.Tags.shadow_type ->
  ?relief:Gtk.Tags.relief_style ->
  ?size_points:float ->
  ?packing:(GObj.widget -> unit) ->
  unit ->
  object
    val obj : Gtk.widget Gtk.obj
    val tag_select : GText.tag
    val tag_select_start : GText.mark
    val tag_select_stop : GText.mark
    val mutable tagfgcolor : (string * GText.tag) list
    val mutable tagsize : (float * GText.tag) list
    method private apply_color_fg : unit -> unit
    method as_widget : Gtk.widget Gtk.obj
    method buffer : GText.buffer
    method button_bold : GButton.toggle_button
    method button_center : GButton.toggle_button
    method button_clear : GButton.button
    method button_fgcolor : GButton.color_button
    method button_italic : GButton.toggle_button
    method button_left : GButton.toggle_button
    method button_right : GButton.toggle_button
    method button_uline : GButton.toggle_button
    method coerce : GObj.widget
    method connect : signals
    method destroy : unit -> unit
    method private do_set_markup : string -> unit
    method drag : GObj.drag_ops
    method entry_size : GEdit.spin_button
    method private find_tagfgcolor : string -> GText.tag
    method private find_tagsize : float -> GText.tag
    method get_markup : unit -> string
    method get_oid : int
    method private get_range_for_tag : unit -> GText.iter * GText.iter
    method misc : GObj.misc_ops
    method private set_align : float ref -> GText.tag list -> unit
    method set_color_fg : string -> unit
    method private set_fgcolor : string option ref -> GText.tag list -> unit
    method set_markup : string -> unit
    method private set_size : float option ref -> GText.tag list -> unit
    method private set_style : string list ref -> GText.tag list -> unit
    method toolbox : GPack.box
    method toolbox_secondary : GPack.box
    method view : GText.view
  end
and changed :
  unit ->
  object
    val mutable callbacks : (GtkSignal.id * (unit -> unit)) list
    method call : unit -> unit
    method callbacks : (GtkSignal.id * (unit -> unit)) list
    method connect : after:bool -> callback:(unit -> unit) -> GtkSignal.id
    method disconnect : GtkSignal.id -> bool
  end
and markup_set :
  unit ->
  object
    val mutable callbacks : (GtkSignal.id * (string -> unit)) list
    method call : string -> unit
    method callbacks : (GtkSignal.id * (string -> unit)) list
    method connect : after:bool -> callback:(string -> unit) -> GtkSignal.id
    method disconnect : GtkSignal.id -> bool
  end
and signals :
  changed:changed ->
  markup_set:markup_set ->
  object ('a)
    val after : bool
    val mutable disconnectors : (GtkSignal.id -> bool) list
    method after : 'a
    method changed : callback:(unit -> unit) -> GtkSignal.id
    method disconnect : GtkSignal.id -> unit
    method markup_set : callback:(string -> unit) -> GtkSignal.id
  end
