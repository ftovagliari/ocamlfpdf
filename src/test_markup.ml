open Printf

let text = "\
Returns whether the character at iter is within an editable region of text. Non-editable text is \"locked\" and can't be changed by the user via GtkTextView. This function is simply a convenience wrapper around gtk_text_iter_get_attributes(). If no tags applied to this text affect editability, default_setting will be returned.

You don't want to use this function to decide whether text can be inserted at iter, because for insertion you don't want to know whether the char at iter is inside an editable range, you want to know whether a new character inserted at iter would be inside an editable range. Use gtk_text_iter_can_insert() to handle this case."

type t = Span of string list | End_of_span | Text of string
type attr = {mutable style : string list; size : float}

let remove_dupl l =
  List.rev (List.fold_left (fun acc y -> if List.mem y acc then acc else y :: acc) [] l)


class editor ~buffer ~size ?packing () =
  let default_size      = size in
  let vbox              = GPack.vbox ?packing () in
  let toolbox           = GPack.hbox ~packing:vbox#pack () in
  let adjustment        = GData.adjustment ~lower:3.5 ~upper:50. ~step_incr:0.5 ~page_size:0.0 () in
  let entry_size        = GEdit.spin_button ~adjustment ~numeric:true ~digits:1 ~value:size  ~packing:toolbox#pack () in
  let button_bold       = GButton.toggle_button ~packing:toolbox#pack () in
  let _                 = button_bold#set_image (GMisc.image ~stock:`BOLD ~icon_size:`MENU ())#coerce in
  let button_italic     = GButton.toggle_button ~packing:toolbox#pack () in
  let _                 = button_italic#set_image (GMisc.image ~stock:`ITALIC ~icon_size:`MENU ())#coerce in
  let button_uline      = GButton.toggle_button ~packing:toolbox#pack () in
  let _                 = button_uline#set_image (GMisc.image ~stock:`UNDERLINE ~icon_size:`MENU ())#coerce in
  let button_fgcolor    = GButton.color_button ~color:(GDraw.color `BLACK) ~title:"Colore del testo" ~packing:toolbox#pack () in
  let button_bgcolor    = GButton.color_button ~color:(GDraw.color `WHITE) ~title:"Colore di sfondo del testo" ~packing:toolbox#pack () in
  let button_base_color = GButton.color_button ~color:(GDraw.color `WHITE) ~title:"Colore di sfondo del riquadro" ~packing:toolbox#pack () in
  let sw                = GBin.scrolled_window ~width:500 ~height:300 ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let view              = GText.view ~wrap_mode:`WORD ~buffer () in
  let _                 = sw#add view#coerce in
  let tag_bold          = buffer#create_tag [`WEIGHT `BOLD] in
  let tag_italic        = buffer#create_tag [`STYLE `ITALIC] in
  let tag_uline         = buffer#create_tag [`UNDERLINE `LOW] in
  let tag_fgcolor       = buffer#create_tag [`FOREGROUND "#000000" ] in
  let tag_bgcolor       = buffer#create_tag [`BACKGROUND "#ffffff"] in
  let _                 =
    button_bold#misc#set_can_focus false;
    button_italic#misc#set_can_focus false;
    button_uline#misc#set_can_focus false;
  in
object (self)
    inherit GObj.widget vbox#as_widget
    val mutable tagsize = []
    val tag_select = buffer#create_tag [`BACKGROUND_GDK (view#misc#style#bg `SELECTED) ]
    val tag_select_start = `MARK (buffer#create_mark buffer#start_iter)
    val tag_select_stop = `MARK (buffer#create_mark buffer#start_iter)
    method get_oid = 0

    method private find_tagsize size =
      try List.assoc size tagsize
      with Not_found -> begin
        let tag = buffer#create_tag [`SIZE_POINTS size] in
        tagsize <- (size, tag) :: tagsize;
        tag
      end

    method get_markup () =
      let iter = ref buffer#start_iter in
      let buf = ref [] in
      let style = ref [] in
      let tags = ref [] in
      while not !iter#is_end do
        tags := List.filter (fun t -> not (!iter#ends_tag (Some t))) !tags;
        tags := !tags @ (List.filter (fun t -> !iter#begins_tag (Some t)) !iter#tags);
        if !tags <> [] then begin

          style := List.map begin fun t ->
            if t = tag_bold then "bold"
            else if t = tag_italic then "italic"
            else if t = tag_uline then "underline"
            else ""
          end !tags;
          style := List.filter ((<>) "") !style;
          style := remove_dupl !style;


        end;

        let stop = !iter#forward_to_tag_toggle None in
        let text = !iter#get_text ~stop in
        buf := (Text text) :: !buf;
        iter := stop
      done;
      let elements = List.rev !buf in
      let buf = Buffer.create 1000 in
      List.iter begin function
        | Text text -> Buffer.add_string buf text
        | Span attrs -> Buffer.add_string buf "<SPAN>"
        | End_of_span -> Buffer.add_string buf "</SPAN>"
      end elements;
      Buffer.contents buf

    initializer
      view#set_left_margin 3;
      view#set_right_margin 3;
      view#set_pixels_above_lines 10;
      view#set_pixels_below_lines 10;
      (*Gobject.Property.set tag_select#as_tag {Gobject.name="background-full-height"; conv=Gobject.Data.boolean} true;
      Gobject.Property.set tag_select#as_tag {Gobject.name="background-full-height-set"; conv=Gobject.Data.boolean} true;
      Gobject.Property.set tag_select#as_tag {Gobject.name="paragraph-background"; conv=Gobject.Data.string} "#ff0000";*)
      kprintf view#misc#modify_font_by_name "%f" size;
      tag_select#set_priority 0;
      (** Toggle buttons *)
      let connect_toggle (button, tag) =
        let sign = button#connect#clicked ~callback:begin fun () ->
          let start = buffer#get_iter_at_mark tag_select_start in
          let stop = buffer#get_iter_at_mark tag_select_stop in
          if not (start#equal stop) then begin
            if button#active then (buffer#apply_tag tag ~start ~stop) else (buffer#remove_tag tag ~start ~stop)
          end
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
            if entry_size#value <> default_size then begin
              let tag = self#find_tagsize entry_size#value in
              buffer#apply_tag tag ~start ~stop
            end
          end
      end);

end

let main () = begin
  let window = GWindow.window ~position:`CENTER ~show:false () in
  let vbox = GPack.vbox ~packing:window#add () in
  let buffer = GText.buffer ~text () in
  let editor = new editor ~buffer ~size:10. ~packing:vbox#add () in
  let button_ok = GButton.button ~stock:`OK ~packing:vbox#pack () in
  button_ok#connect#clicked ~callback:begin fun () ->
    Printf.printf "%s\n%!" (editor#get_markup());
  end;
  (*  *)
  window#connect#destroy ~callback:GMain.quit;
  window#show();
  GMain.main()
end

let _ = main ()
