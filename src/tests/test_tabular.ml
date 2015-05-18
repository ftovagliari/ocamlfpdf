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

open Printf

let (//) = Filename.concat
let main () = begin
  let filename = Sys.argv.(0) ^ ".pdf" in
  let outchan = open_out_bin filename in
  let close_file () = close_out outchan in
  begin
    try
      let doc = Fpdf.create ~orientation:`Landscape ~outchan () in
      Fpdf.set_auto_page_break false doc;
      Fpdf_font.embed_font ~family:`CenturySchoolbook ~style:[] doc;
      Fpdf_font.embed_font ~family:`CenturySchoolbook ~style:[`Italic] doc;
      Fpdf_font.embed_font ~family:`CenturySchoolbook ~style:[`Bold] doc;
      Fpdf_font.embed_font ~family:`CenturySchoolbook ~style:[`Bold; `Italic] doc;
      Fpdf_font.embed_font ~family:`CMUSerif ~style:[] doc;
      Fpdf_font.embed_font ~family:`CMUSerif ~style:[`Bold] doc;
      Fpdf_font.embed_font ~family:`CMUSerif ~style:[`Italic] doc;
      Fpdf_font.embed_font ~family:`CMUSerif ~style:[`Bold; `Italic] doc;
      Fpdf_font.embed_font ~family:`CMUSansSerif ~style:[] doc;
      Fpdf_font.embed_font ~family:`CMUSansSerif ~style:[`Bold] doc;
      Fpdf_font.embed_font ~family:`CMUSansSerif ~style:[`Italic] doc;
      Fpdf_font.embed_font ~family:`CMUSansSerif ~style:[`Bold; `Italic] doc;
      let font_size     = 9.0 in
      let small_font    = font_size -. 2.0 in
      let line_spacing  = 1.5 in
      let margin        = 10. in
      let margin_top    = margin in
      let margin_right  = margin in
      let margin_bottom = margin in
      let margin_left   = margin in
      let width_avail   = Fpdf_page.page_width doc -. margin_left -. margin_right in
      let height_avail  = Fpdf_page.page_height doc -. margin_top -. margin_bottom in
      Fpdf.set_margins ~top:margin_top ~right:margin_right ~bottom:margin_bottom ~left:margin_left doc;


      (**  *)
      Fpdf.add_page doc;
      Fpdf.set_font ~family:`CMUSerif ~size:font_size doc;
      let x = margin_left in

      let y0 = margin_top +. 50. in
      let colwidths = [| 2.0; 15.; 7.5; 5.5; 11.0; 7.0; 11.0; 7.0; 11.0; 7.0; 7.; 9. |] in
      let table = Fpdf_tabular.create ~x ~y:y0 ~border_width:`Thick ~padding:0.0 ~width:width_avail ~colwidths ~debug:false doc in

      let set = Fpdf_tabular.set_markup table in
      let markup ?(style="") ?(align=0.5) ?(family="CMUSerif") ?char_space ?font_scale ?font_size text =
        sprintf "<SPAN family='%s' align='%f' style='%s'%s%s%s>%s</SPAN>"
          family align style
          (match char_space with Some x -> sprintf " char_space='%f'" x | _ -> "")
          (match font_scale with Some x -> sprintf " scale='%d'" x | _ -> "")
          (match font_size with Some x -> sprintf " size='%f'" x | _ -> "")
          text
      in

      (*  *)

      set 0 0 ~rowspan:3 ~colspan:2 (markup ~char_space:2. ~font_scale:105 "GRANDEZZE");
      set 0 2 ~rowspan:3 (markup "Simbolo\ne\nlegame dimensionale");
      set 0 3 ~rowspan:3 (markup ~font_scale:90 "Dimensioni (1)");

      set 0 4 ~colspan:2 ~padding:(1.5,0.,1.5,0.) (markup "SISTEMA GIORGI (M. K. S.)");
      set 1 4 ~colspan:2 ~padding:(1.5,0.,1.5,0.) (sprintf "<SPAN align='0.5'>UNIT\192 FONDAMENTALI</SPAN><BR/><SPAN align='0.5' size='%f'>metro, chilogrammo (massa), secondo</SPAN>" small_font);
      set 2 4 ~colspan:1 (markup ~font_size:small_font "Denominazione{Prov\199}\192");
      set 2 5 ~colspan:1 (markup ~font_size:small_font "Abbreviazione");

      set 0 6 ~colspan:2 (markup "SISTEMA TECNICO");
      set 1 6 ~colspan:2 (markup "UNIT\192 FONDAMENTALI\nmetro, chilogrammo (peso), secondo");
      set 2 6 ~colspan:1 (markup ~font_size:small_font "Denominazione");
      set 2 7 ~colspan:1 (markup ~font_size:small_font "Abbreviazione");

      set 0 8 ~colspan:2 (markup "SISTEMA C. G. S.");
      set 1 8 ~colspan:2 (markup "UNIT\192 FONDAMENTALI<BR/>centimetro, grammo (massa), secondo");
      set 2 8 ~colspan:1 (markup ~font_size:small_font "Denominazione");
      set 2 9 ~colspan:1 (markup ~font_size:small_font "Abbreviazione");

      set 0 10 ~colspan:2 ~rowspan:2
        (sprintf
           "<SPAN align='0.5'>EQUIVALENZA</SPAN><BR/><SPAN size='%f' align='0.5'>tra le unit\224 del </SPAN><SPAN style='italic' size='%f' align='0.5'>sist. Giorgi</SPAN><BR/><SPAN align='0.5' size='%f'>e le corrispondenti<BR/>del </SPAN><SPAN style='italic' size='%f'>sist. C. G. S.</SPAN>"
           small_font small_font small_font small_font);
      set 2 10 (markup ~font_size:small_font "Sist. Giorgi");
      set 2 11 (markup ~font_size:small_font "Sist. C.G.S.");

      (*  *)

      set 3 0 ~rowspan:4 (markup "G");
      set 3 1 (markup ~style:"italic" ~align:0.0 "Lunghezza");
      set 4 1 (markup ~style:"italic" ~align:0.0 "Area");
      set 5 1 (markup ~style:"italic" ~align:0.0 "Volume");
      set 6 1 (markup ~style:"italic" ~align:0.0 "Angolo piano");

      set 3 2 (markup ~align:0.0 "l");
      set 4 2 (markup ~align:0.0 "A = l\178");
      set 5 2 (markup ~align:0.0 "V = l\179");
      set 6 2 (markup ~align:0.0 "alpha");

      set 3 3 (markup ~align:0.0 "L");
      set 4 3 (markup ~align:0.0 "L\178");
      set 5 3 (markup ~align:0.0 "L\179");
      set 6 3 (sprintf "L<SPAN rise='2.85' size='%f' scale='120' style='bold'>0</SPAN> (\178)" (font_size *. 0.525));

      set 3 4 (markup ~align:0.0 "metro");
      set 4 4 (markup ~align:0.0 "metro quadrato");
      set 5 4 (markup ~align:0.0 "metro cubo");
      set 6 4 (markup ~align:0.0 "radiante");

      set 3 5 (markup ~align:0.0 "m");
      set 4 5 (markup ~align:0.0 "m\178");
      set 5 5 (markup ~align:0.0 "m\179");
      set 6 5 (markup ~align:0.0 "r");

      set 3 6 (markup ~align:0.0 "");
      set 4 6 (markup ~align:0.0 "");
      set 5 6 (markup ~align:0.0 "");
      set 6 6 (markup ~align:0.0 "");

      set 3 7 (markup ~align:0.0 "");
      set 4 7 (markup ~align:0.0 "");
      set 5 7 (markup ~align:0.0 "");
      set 6 7 (markup ~align:0.0 "");

      set 3 8 (markup ~align:0.0 "");
      set 4 8 (markup ~align:0.0 "");
      set 5 8 (markup ~align:0.0 "");
      set 6 8 (markup ~align:0.0 "");

      set 3 9 (markup ~align:0.0 "");
      set 4 9 (markup ~align:0.0 "");
      set 5 9 (markup ~align:0.0 "");
      set 6 9 (markup ~align:0.0 "");

      set 3 10 (markup ~align:0.0 "");
      set 4 10 (markup ~align:0.0 "");
      set 5 10 (markup ~align:0.0 "");
      set 6 10 (markup ~align:0.0 "");

      set 3 11 (markup ~align:0.0 "");
      set 4 11 (markup ~align:0.0 "");
      set 5 11 (markup ~align:0.0 "");
      set 6 11 (markup ~align:0.0 "");


      let last_h = ref 0. in
      let count_table_pages = ref 0 in
      let header_height = 30. in
      let y_max = margin_top +. height_avail in
      let current_table_height = ref (Fpdf_tabular.table_height table) in

      for i = 7 to 300 do
        let origin = if !count_table_pages = 0 then y0 else margin_top +. header_height in
        set i 0 (kprintf markup "%d" i);
        set i 1 (markup "A");
        set i 2 (markup "A");
        set i 3 (markup "A");
        set i 4 (markup "A");
        set i 5 (markup "A");
        set i 6 (markup "A");
        set i 7 (markup "A");
        set i 8 (markup "A");
        set i 9 (markup "A");
        set i 10 (markup "A");
        set i 11 (markup "A");
        let current_row_height = Fpdf_tabular.row_height table i in
        current_table_height := !current_table_height +. current_row_height;
        let cur_height_in_page = !current_table_height (*Fpdf_tabular.table_height table*) -. !last_h in
        let yy = origin +. cur_height_in_page in
        (*Printf.printf "i=%d; cur_height_in_page=%f; yy=%f; count_table_pages=%d; origin:=%f; Y-MAX=%f; last_h=%f\n%!"
          i cur_height_in_page yy !count_table_pages origin y_max !last_h;*)
        if yy > y_max then begin
          (*Printf.printf "------------------------------------------\n%!" ;*)
          incr count_table_pages;
          last_h := !last_h +. cur_height_in_page -. current_row_height;
          Fpdf_tabular.add_page_break_before i table (fun () -> Some (margin_top +. header_height));
        end
      done;

      (*Fpdf_tabular.add_vertical_line ~rowstart:3 ~col:1 table;
      Fpdf_tabular.add_vertical_line ~rowstart:0 ~col:2 table;
      Fpdf_tabular.add_vertical_line ~rowstart:0 ~col:3 table;
      Fpdf_tabular.add_vertical_line ~rowstart:0 ~col:4 table;
      Fpdf_tabular.add_vertical_line ~rowstart:2 ~col:5 table;
      Fpdf_tabular.add_vertical_line ~rowstart:0 ~col:6 table;
      Fpdf_tabular.add_vertical_line ~rowstart:2 ~col:7 table;
      Fpdf_tabular.add_vertical_line ~rowstart:0 ~col:8 table;
      Fpdf_tabular.add_vertical_line ~rowstart:2 ~col:9 table;
      Fpdf_tabular.add_vertical_line ~rowstart:0 ~col:10 table;
      Fpdf_tabular.add_vertical_line ~rowstart:2 ~col:11 table;*)

      (*Fpdf_tabular.add_horizontal_line ~colstart:1 ~colstop:7 ~row:25 table;
      Fpdf_tabular.add_horizontal_line ~colstart:1 ~colstop:8 ~row:35 table;*)
      Fpdf_tabular.add_vertical_line ~line_width:`Medium ~rowstart:3 ~rowstop:100 ~col:2 table;
      (*Fpdf_tabular.add_vertical_line ~rowstart:20 ~rowstop:40 ~col:2 table;*)

      (*Fpdf_tabular.add_horizontal_line ~colstart:4 ~colstop:8 ~row:1 table;
      Fpdf_tabular.add_horizontal_line ~colstart:4 ~colstop:11 ~row:2 table;
      Fpdf_tabular.add_horizontal_line ~colstart:0 ~row:3 table;*)

      Fpdf_tabular.pack table;

      (*let line_spacing = None in
      Fpdf.set_font ~family:`Times ~style:[`Bold] doc;
      let markup = Fpdf_markup.prepare ~markup:"<span align='0.0' size='5' style='bold,italic'>{Prov\199}\192\n}\192}\192}\192}\192}\192}\192</span>"
          ~width:100. ~border_width:0.1 ~border_radius:5. ?line_spacing doc
      in
      markup.Fpdf_markup.print~x:100. ~y:100. (*~valign:()*) ();

      let markup = Fpdf_markup.prepare ~markup:"<span align='0.0' size='12' style='bold,italic'>{Prov\199}\192\n}\192}\192}\192}\192}\192}\192</span>"
          ~width:100. ~border_width:0.1 ~border_radius:5. ?line_spacing doc
      in
      markup.Fpdf_markup.print~x:100. ~y:120. (*~valign:()*) ();

      let markup = Fpdf_markup.prepare ~markup:"<span align='0.0' size='12' style='bold,italic'>{AAAA\192</span>"
          ~width:100. ~border_width:5. ~border_radius:5. ~border_color:"#ff0000" ?line_spacing doc
      in
      markup.Fpdf_markup.print~x:0. ~y:0. (*~valign:()*) ();


      let markup = Fpdf_markup.prepare ~markup:"<span align='0.0' size='25'>{Prov\199}\192}\192}\192\n}\192}\192}\192}\192</span>"
          ~width:100. ~border_width:0.1 ~border_radius:5. ?line_spacing doc
      in
      markup.Fpdf_markup.print~x:100. ~y:140. (*~valign:()*) ();

      Fpdf.add_page doc;
      Fpdf.set ~x:0. ~y:0. doc;
      Fpdf.set_font ~family:`Times ~size:100. doc;
      Fpdf.cell ~width:50. ~text:"A{\199\199\199\199\199(0,0)}\192" ~border:[`All] doc;*)

     (* let lh = Fpdf.font_size doc /. Fpdf.scale doc *. 1.125 in
      Printf.printf "---> lh = %f\n%!" lh;
      Fpdf.set ~x:0. ~y:lh doc;
      Fpdf.cell ~width:50. ~text:"A{Prova(0,0)}\192" doc;
      Fpdf.rect ~x:0. ~y:0. ~width:20. ~height:(Fpdf.font_size doc /. Fpdf.scale doc) doc;
*)

      (**  *)
      Fpdf.close_document doc;
      close_file();
    with ex -> begin
      close_file();
      raise ex
    end
  end;
  Printf.printf "
  +-----------------------------------------------------------------------------
  |
  | Please see output file %s
  |
  +-----------------------------------------------------------------------------

%!" filename;
  if Sys.os_type = "Win32" then ignore (Sys.command filename)
  else ignore (kprintf Sys.command "xpdf %s" filename)
end

let _ = Fpdf_error.handle_error main ()
