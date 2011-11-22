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

let text = "\
Returns whether the character at iter is within an editable   region of text. Non-editable text is locked and can't be changed by the user via GtkTextView. This function is simply a convenience wrapper around gtk_text_iter_get_attributes(). If no tags applied to this text affect editability, default_setting will be returned.

You don't want to use this function to decide whether text can be@inserted at iter, because for insertion you don't want to know whether the char at iter is inside an editable range, you want to know whether a new character inserted at iter would be inside an editable range. Use gtk_text_iter_can_insert() to handle this case."


let main () = begin
  let window = GWindow.window ~position:`CENTER ~show:false () in
  let vbox = GPack.vbox ~packing:window#add () in
  let buffer = GText.buffer ~text () in
  let editor = new Gtk_pdfmarkup_editor.editor ~buffer ~size:10. ~packing:vbox#add () in
  let markup = text in
  let markup = Str.global_replace (Str.regexp_string "function") "<SPAN size='18'>function</SPAN>" markup in
  let markup = Str.global_replace (Str.regexp_string "text") "<SPAN size='18' style='bold'>text</SPAN>" markup in
  let markup = Str.global_replace (Str.regexp_string "region") "<SPAN style='bold,italic,underline'>region</SPAN>" markup in
  let markup = Str.global_replace (Str.regexp_string "editable") "<SPAN size='12' style='bold,italic,underline'>editable</SPAN>" markup in
  let markup = Str.global_replace (Str.regexp_string "@") "<BR/>" markup in
  let markup = "<SPAN>aaa </SPAN><SPAN size='18.5'>bbbbb</SPAN><SPAN> cccc

kdfssfhsjkfjksf</SPAN>" in
  editor#set_markup markup;
  let button_ok = GButton.button ~stock:`OK ~packing:vbox#pack () in
  button_ok#connect#clicked ~callback:begin fun () ->
    Printf.printf "%s\n%!" (editor#get_markup());
  end;
  (*  *)
  window#connect#destroy ~callback:GMain.quit;
  window#show();
  GMain.main()
end
let _ = Printexc.record_backtrace true
let _ = main ()




