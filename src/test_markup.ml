open Printf

let text = "\
Returns whether the character at iter is within an editable region of text. Non-editable text is \"locked\" and can't be changed by the user via GtkTextView. This function is simply a convenience wrapper around gtk_text_iter_get_attributes(). If no tags applied to this text affect editability, default_setting will be returned.

You don't want to use this function to decide whether text can be inserted at iter, because for insertion you don't want to know whether the char at iter is inside an editable range, you want to know whether a new character inserted at iter would be inside an editable range. Use gtk_text_iter_can_insert() to handle this case."

class editor ~buffer ~size ?packing () =
  let vbox = GPack.vbox ?packing () in
  let toolbox = GPack.hbox ~packing:vbox#pack () in
  let adjustment = GData.adjustment ~lower:3.5 ~upper:50. ~step_incr:0.5 () in
  let entry_size = GEdit.spin_button ~adjustment ~numeric:true ~digits:1 ~value:size ~packing:toolbox#pack () in
  let button_bold = GButton.toggle_button ~stock:`BOLD ~packing:toolbox#pack () in
  let button_italic = GButton.toggle_button ~stock:`ITALIC ~packing:toolbox#pack () in
  let button_uline = GButton.toggle_button ~stock:`UNDERLINE ~packing:toolbox#pack () in
  let button_fgcolor = GButton.color_button ~color:(GDraw.color `BLACK) ~title:"Colore del testo" ~packing:toolbox#pack () in
  let button_bgcolor = GButton.color_button ~color:(GDraw.color `WHITE) ~title:"Colore di sfondo del testo" ~packing:toolbox#pack () in
  let button_base_color = GButton.color_button ~color:(GDraw.color `WHITE) ~title:"Colore di sfondo del riquadro" ~packing:toolbox#pack () in
  let sw = GBin.scrolled_window ~width:500 ~height:300 ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let view = GText.view ~wrap_mode:`WORD ~buffer () in
  let _ = sw#add view#coerce in
  let tag_bold = buffer#create_tag [`WEIGHT `BOLD] in
  let tag_italic = buffer#create_tag [`STYLE `ITALIC] in
  let tag_uline = buffer#create_tag [`UNDERLINE `LOW] in
  let tag_fgcolor = buffer#create_tag [`FOREGROUND "#000000" ] in
  let tag_bgcolor = buffer#create_tag [`BACKGROUND "#ffffff" ] in
  let tag_size = [] in
object (self)
    inherit GObj.widget vbox#as_widget 
    method get_oid = 0
    initializer
      view#set_left_margin 3;
      view#set_right_margin 3;
      kprintf view#misc#modify_font_by_name "%f" size;
      (** Toggle buttons *)
      let connect_toggle (button, tag) =
        let sign = button#connect#clicked ~callback:begin fun () ->
          let start, stop = buffer#selection_bounds in
          if button#active then (buffer#apply_tag tag ~start ~stop) else (buffer#remove_tag tag ~start ~stop)
        end in
        button, sign, tag
      in
      let toggles = List.map connect_toggle [button_bold, tag_bold; button_italic, tag_italic; button_uline, tag_uline] in
      ignore (buffer#connect#after#mark_set ~callback:begin fun iter mark ->
        let update (button, signal, tag) =
          let start, stop = buffer#selection_bounds in
          let has = ref (start#has_tag tag) in
          let start = ref start in
          while not (!start#equal stop) do
            has := !has && (!start#has_tag tag);
            start := !start#forward_char;
          done;
          button#misc#handler_block signal;
          button#set_active !has;
          button#misc#handler_unblock signal;
        in
        List.iter update toggles
      end);
      (** Font size *)
      ignore (entry_size#connect#value_changed ~callback:begin fun () ->
        let start, stop = buffer#selection_bounds in () 
        (*if entry_size#value = size then (buffer#remove_tag tag_size ~start ~stop)
        else begin
          tag_size#set_properties [`SIZE_POINTS entry_size#value];
          buffer#apply_tag tag_size ~start ~stop
        end*)
      end);
     
end
   
let main () = begin
  let window = GWindow.window ~position:`CENTER ~show:false () in
  let buffer = GText.buffer ~text () in  
  let editor = new editor ~buffer ~size:10. ~packing:window#add () in
  (*  *)
  window#connect#destroy ~callback:GMain.quit;
  window#show();
  GMain.main()
end

let _ = main ()