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


open Printf

let remove_dupl l =
  List.rev (List.fold_left (fun acc y -> if List.mem y acc then acc else y :: acc) [] l)

let filter_map p =
  let rec find accu = function
  | [] -> List.rev accu
  | x :: l ->
    find begin
      match p x with
        | None -> accu
        | Some m -> (m :: accu)
    end l
  in
  find [];;

let list_full x n =
  let seq = ref [] in
  for i = 1 to n do seq := x :: !seq done;
  !seq;;

let trim =
  let re = Str.regexp "\\(^[ ]+\\)\\|\\([ ]+$\\)" in
  Str.global_replace re ""

let rgb_of_hex name = Scanf.sscanf name "#%2x%2x%2x" (fun r g b -> (r, g, b));;
let hex_of_rgb (r, g, b) = Printf.sprintf "#%02X%02X%02X" r g b;;

let name = function `RGB (red, green, blue) -> Printf.sprintf "#%X%X%X" red green blue

let name_of_gdk color =
  let r = Gdk.Color.red color in
  let g = Gdk.Color.green color in
  let b = Gdk.Color.blue color in
  hex_of_rgb (r * 255/65535, g * 255/65535, b * 255/65535)


class changed () = object (self) inherit [unit] GUtil.signal () as super end

and signals ~changed =
  object (self)
    inherit GUtil.ml_signals [changed#disconnect]
    method changed = changed#connect ~after
  end

and editor ~buffer ~size_points ?width ?height ?shadow_type ?packing () =
  let changed           = new changed () in
  let default_fgcolor   = "#000000" in
  let vbox              = GPack.vbox ?packing () in
  let toolbox           = GPack.hbox ~packing:vbox#pack () in
  let adjustment        = GData.adjustment ~lower:3.5 ~upper:50. ~step_incr:0.5 ~page_size:0.0 () in
  let entry_size        = GEdit.spin_button ~adjustment ~numeric:true ~digits:1 ~value:size_points  ~packing:toolbox#pack () in
  let button_bold       = GButton.toggle_button ~packing:toolbox#pack () in
  let _                 = button_bold#set_image (GMisc.image ~stock:`BOLD ~icon_size:`MENU ())#coerce in
  let button_italic     = GButton.toggle_button ~packing:toolbox#pack () in
  let _                 = button_italic#set_image (GMisc.image ~stock:`ITALIC ~icon_size:`MENU ())#coerce in
  let button_uline      = GButton.toggle_button ~packing:toolbox#pack () in
  let _                 = button_uline#set_image (GMisc.image ~stock:`UNDERLINE ~icon_size:`MENU ())#coerce in
  let button_fgcolor    = GButton.color_button ~color:(GDraw.color `BLACK) ~title:"Colore del testo" ~packing:toolbox#pack () in
  let button_bgcolor    = GButton.color_button ~color:(GDraw.color `WHITE) ~title:"Colore di sfondo del testo" ~packing:toolbox#pack ~show:false () in
  let button_base_color = GButton.color_button ~color:(GDraw.color `WHITE) ~title:"Colore di sfondo del riquadro" ~packing:toolbox#pack ~show:false () in
  let button_clear      = GButton.button ~packing:toolbox#pack () in
  let _                 = button_clear#set_image (GMisc.image ~stock:`CLEAR ~icon_size:`MENU ())#coerce in
  let sw                = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add ?shadow_type () in
  let view              = GText.view ~wrap_mode:`WORD ?width ?height ~buffer () in
  let _                 = sw#add view#coerce in
  let tag_bold          = buffer#create_tag [`WEIGHT `BOLD] in
  let tag_italic        = buffer#create_tag [`STYLE `ITALIC] in
  let tag_uline         = buffer#create_tag [`UNDERLINE `LOW] in
  let tag_fgcolor       = buffer#create_tag [`FOREGROUND default_fgcolor ] in
  let tag_bgcolor       = buffer#create_tag [`BACKGROUND "#ffffff"] in
  let _                 =
    button_bold#misc#set_can_focus false;
    button_italic#misc#set_can_focus false;
    button_uline#misc#set_can_focus false;
  in
object (self)
    inherit GObj.widget vbox#as_widget
    val mutable tagsize = []
    val mutable tagfgcolor = []
    val tag_select = buffer#create_tag [`BACKGROUND_GDK (view#misc#style#bg `SELECTED) ]
    val tag_select_start = `MARK (buffer#create_mark buffer#start_iter)
    val tag_select_stop = `MARK (buffer#create_mark buffer#start_iter)
    method get_oid = 0

    method connect = new signals ~changed

    method view = view
    method buffer = buffer

    method private find_tagsize size =
      try List.assoc size tagsize
      with Not_found -> begin
        let tag = buffer#create_tag [`SIZE_POINTS size] in
        tagsize <- (size, tag) :: tagsize;
        tag
      end

    method private find_tagfgcolor color =
      try List.assoc color tagfgcolor
      with Not_found -> begin
        let tag = buffer#create_tag [`FOREGROUND color] in
        tagfgcolor <- (color, tag) :: tagfgcolor;
        tag
      end

    method set_markup markup =
      buffer#delete buffer#start_iter buffer#end_iter;
      let markup = Str.global_replace (Str.regexp "\n") "<BR/>" markup in
      let xml = Xml.parse_string ("<MARKUP>" ^ markup ^ "</MARKUP>") in
      (*let s = Xml.to_string_fmt xml in
      Printf.printf "%s\n%!" s;*)
      Xml.iter begin function
        | Xml.PCData "&nbsp;" ->
          buffer#insert ~tags:[] " ";
        | Xml.PCData text ->
          buffer#insert ~tags:[] text;
        | Xml.Element (tag, _, _) when (String.lowercase tag) = "br" ->
          buffer#insert ~tags:[] "\n";
        | Xml.Element (tag, attrs, children) when (String.lowercase tag) = "span" ->
          (** size *)
          let size =
            try
              let n = float_of_string (List.assoc "size" attrs) in
              Some (self#find_tagsize n)
            with Not_found -> None
          in
          (** fgcolor *)
          let fgcolor =
            try
              let n = List.assoc "color" attrs in
              Some (self#find_tagfgcolor n)
            with Not_found -> None
          in
          (** styles *)
          let styles =
            try
              let style = List.assoc "style" attrs in
              Str.split (Str.regexp ",") style
            with Not_found -> []
          in
          let styles = List.map begin function
            | "bold" -> Some tag_bold
            | "italic" -> Some tag_italic
            | "underline" -> Some tag_uline
            | _ -> assert false
          end styles in
          (** underline *)
          let underline =
            try
              ignore (List.assoc "underline" attrs);
              Some tag_uline
            with Not_found -> None
          in
          (**  *)
          let tags = filter_map (fun x -> x) (size :: fgcolor :: underline :: styles) in
          begin
            match children with
              | [] -> buffer#insert ~tags " "
              | children ->
                List.iter begin function
                  | Xml.PCData text -> buffer#insert ~tags text
                  | Xml.Element (tag, _, _) when (String.lowercase tag) = "br" -> buffer#insert ~tags "\n"
                  | _ -> failwith "invalid_markup"
                end children
          end;
        | Xml.Element (tag, _, []) ->
          buffer#insert ~tags:[] "";
        | _ -> failwith "invalid_markup"
      end xml

    method get_markup () =
      let iter = ref buffer#start_iter in
      let buf = Buffer.create 1000 in
      let current_style = ref [] in
      let current_size = ref None in
      let current_fgcolor = ref None in
      let current_uline = ref false in
      let current_tags = ref [] in
      while not !iter#is_end do
        current_tags := List.filter (fun t -> not (!iter#ends_tag (Some t))) !current_tags;
        current_tags := !current_tags @ (List.filter (fun t -> !iter#begins_tag (Some t)) !iter#tags);
        (** Style *)
        current_style := List.map begin fun t ->
          if t#get_oid = tag_bold#get_oid then "bold"
          else if t#get_oid = tag_italic#get_oid then "italic"
          else ""
        end !current_tags;
        current_style := List.filter ((<>) "") !current_style;
        current_style := remove_dupl !current_style;
        (** Font size *)
        current_size := begin
          try
            let size, _ = List.find begin fun (size, ts) ->
              try
                ignore (List.find (fun ct -> ts#get_oid = ct#get_oid) !current_tags);
                true
              with Not_found -> false
            end tagsize in
            Some size
          with Not_found -> (Some size_points)
        end;
        (** fgcolor *)
        current_fgcolor := begin
          try
            let color, _ = List.find begin fun (color, ts) ->
              try
                ignore (List.find (fun ct -> ts#get_oid = ct#get_oid) !current_tags);
                true
              with Not_found -> false
            end tagfgcolor in
            Some color
          with Not_found -> None
        end;
        (**  *)
        current_uline := List.exists (fun t -> t#get_oid = tag_uline#get_oid) !current_tags;
        (**  *)
        let stop = !iter#forward_to_tag_toggle None in
        let text = !iter#get_text ~stop in
        Buffer.add_string buf "<SPAN";
        if !current_style <> [] then begin
          Buffer.add_string buf " style='";
          Buffer.add_string buf (String.concat "," !current_style);
          Buffer.add_string buf "'";
          current_style := [];
        end;
        if !current_uline then begin
          Buffer.add_string buf " underline='low'";
          current_uline := false;
        end;
        begin
          match !current_size with
            | Some size ->
              Buffer.add_string buf " size='";
              Buffer.add_string buf (string_of_float size);
              Buffer.add_string buf "'";
              current_size := None;
            | _ -> ()
        end;
        begin
          match !current_fgcolor with
            | Some color ->
              Buffer.add_string buf " color='";
              Buffer.add_string buf color;
              Buffer.add_string buf "'";
              current_fgcolor := None;
            | _ -> ()
        end;
        Buffer.add_string buf ">";
        Buffer.add_string buf text;
        Buffer.add_string buf "</SPAN>";
        iter := stop
      done;
      (*adapt_markup*) (Buffer.contents buf)

    initializer
      view#set_left_margin 5;
      view#set_right_margin 5;
      view#set_pixels_above_lines 1;
      view#set_pixels_below_lines 1;
      (*Gobject.Property.set tag_select#as_tag {Gobject.name="background-full-height"; conv=Gobject.Data.boolean} true;
      Gobject.Property.set tag_select#as_tag {Gobject.name="background-full-height-set"; conv=Gobject.Data.boolean} true;
      Gobject.Property.set tag_select#as_tag {Gobject.name="paragraph-background"; conv=Gobject.Data.string} "#ff0000";*)
      kprintf view#misc#modify_font_by_name "%f" size_points;
      tag_select#set_priority 0;
      ignore (buffer#connect#changed ~callback:changed#call);
      (** Toggle buttons *)
      let connect_toggle (button, tag) =
        let sign = button#connect#clicked ~callback:begin fun () ->
          let start = buffer#get_iter_at_mark tag_select_start in
          let stop = buffer#get_iter_at_mark tag_select_stop in
          if not (start#equal stop) then begin
            if button#active then (buffer#apply_tag tag ~start ~stop) else (buffer#remove_tag tag ~start ~stop)
          end else (button#set_active false);
          changed#call()
        end in
        button, sign, tag
      in
      let toggles = List.map connect_toggle [button_bold, tag_bold; button_italic, tag_italic; button_uline, tag_uline] in
      (** Synchronize tag_select with selection *)
      let busy = ref false in
      let sigid = buffer#connect#mark_set ~callback:begin fun iter mark ->
        match GtkText.Mark.get_name mark with
          | Some name when (not !busy && name = "insert") ->
            busy := true;
            let start, stop = buffer#selection_bounds in
            buffer#remove_tag tag_select ~start:buffer#start_iter ~stop:buffer#end_iter;
            buffer#apply_tag tag_select ~start ~stop;
            buffer#move_mark tag_select_start ~where:start;
            buffer#move_mark tag_select_stop ~where:stop;
            busy := false;
          | _ -> ()
      end in
      (**  *)
      ignore (buffer#connect#after#mark_set ~callback:begin fun iter mark ->
        if not !busy then begin
          let update (button, signal, tag) =
            let start = buffer#get_iter_at_mark tag_select_start in
            let stop = buffer#get_iter_at_mark tag_select_stop in
            if not (start#equal stop) then begin
              let has = ref (start#has_tag tag) in
              let start = ref start in
              while not (!start#equal stop) do
                has := !has && (!start#has_tag tag);
                start := !start#forward_char;
              done;
              button#misc#handler_block signal;
              button#set_active !has;
              button#misc#handler_unblock signal;
            end
          in
          List.iter update toggles
        end
      end);
      (** Font size *)
      ignore (entry_size#connect#value_changed ~callback:begin fun () ->
        let start = buffer#get_iter_at_mark tag_select_start in
        let stop = buffer#get_iter_at_mark tag_select_stop in
        if not (start#equal stop) then begin
          List.iter (fun (_, t) -> buffer#remove_tag t ~start ~stop) tagsize;
          let tag = self#find_tagsize entry_size#value in
          buffer#apply_tag tag ~start ~stop
        end;
        changed#call()
      end);
      (** fgcolor *)
      ignore (button_fgcolor#connect#color_set ~callback:begin fun () ->
        let start = buffer#get_iter_at_mark tag_select_start in
        let stop = buffer#get_iter_at_mark tag_select_stop in
        if not (start#equal stop) then begin
          List.iter (fun (_, t) -> buffer#remove_tag t ~start ~stop) tagfgcolor;
          if name_of_gdk button_fgcolor#color <> default_fgcolor then begin
            let tag = self#find_tagfgcolor (name_of_gdk button_fgcolor#color) in
            buffer#apply_tag tag ~start ~stop
          end
        end;
        changed#call()
      end);
      (**  *)
      ignore (button_clear#connect#clicked ~callback:begin fun () ->
        buffer#remove_all_tags ~start:buffer#start_iter ~stop:buffer#end_iter;
        changed#call()
      end);

end
