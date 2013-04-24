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

open Printf

let (//) = Filename.concat
let main () = begin
  let filename = Sys.argv.(0) ^ ".pdf" in
  let outchan = open_out_bin filename in
  let close_file () = close_out outchan in
  begin
    try
      let doc = PDF.create ~orientation:`Landscape ~outchan () in
      let font_size     = 9.0 in
      let line_spacing  = 1.5 in
      let margin_top    = 10.0 in
      let margin_right  = 10.0 in
      let margin_bottom = 10.0 in
      let margin_left   = 10.0 in
      let width_avail   = PDFPage.page_width doc -. margin_left -. margin_right in
      let height_avail  = PDFPage.page_height doc -. margin_top -. margin_bottom in
      PDF.set_font ~family:`Times ~size:font_size doc;
      PDF.set_margins ~top:margin_top ~right:margin_right ~bottom:margin_bottom ~left:margin_left doc;


      (**  *)
      PDF.add_page doc;
      let width_table = width_avail in
      let (!!) x = width_table *. x /. 100. in
      let x = margin_left in
      let y = margin_top in
      let table = Tabular.create ~x ~y ~border_width:`Thick ~padding:0.75 doc in
      let colwidths = Array.map (!!) [| 2.0; 15.5; 7.5; 5.5; 13.0; 7.0 |] in

      let set = Tabular.set_markup table colwidths in
      let markup ?(style="") ?(align=0.5) ?(family="Times") ?char_space ?font_scale text =
        sprintf "<SPAN family='%s' align='%f' style='%s'%s%s>%s</SPAN>"
          family align style
          (match char_space with Some x -> sprintf " char_space='%f'" x | _ -> "")
          (match font_scale with Some x -> sprintf " scale='%d'" x | _ -> "")
          text
      in

      set 0 0 ~rowspan:3 ~colspan:2 (markup ~char_space:2. ~font_scale:105 "GRANDEZZE");
      set 0 2 ~rowspan:3 (markup "Simbolo\ne\nlegame dimensionale");
      set 0 3 ~rowspan:3 (markup "Dimensioni (1)");

      set 0 4 ~colspan:2 (markup "SISTEMA GIORGI (M. K. S.)");
      set 1 4 ~colspan:2 (markup "UNIT\192 FONDAMENTALI\nmetro, chilogrammo (massa), secondo");
      set 2 4 ~colspan:1 (markup "Denominazione");
      set 2 5 ~colspan:1 (markup "Abbreviazione");

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
      set 6 3 (markup ~align:0.0 "L0 (\178)");

      set 3 4 (markup ~align:0.0 "metro");
      set 4 4 (markup ~align:0.0 "metro quadrato");
      set 5 4 (markup ~align:0.0 "metro cubo");
      set 6 4 (markup ~align:0.0 "radiante");

      set 3 5 (markup ~align:0.0 "m");
      set 4 5 (markup ~align:0.0 "m\178");
      set 5 5 (markup ~align:0.0 "m\179");
      set 6 5 (markup ~align:0.0 "r");

      Tabular.add_vertical_line table 3 1;
      Tabular.add_vertical_line table 0 2;
      Tabular.add_vertical_line table 0 3;
      Tabular.add_vertical_line table 0 4;
      Tabular.add_vertical_line table 2 5;

      Tabular.add_horizontal_line table 0 3;
      Tabular.add_horizontal_line table 4 ~colstop:5 1;
      Tabular.add_horizontal_line table 4 ~colstop:5 2;

      let matrix = Tabular.pack table in



(*      (**  *)
      PDF.add_page doc;
      let x = margin_left in
      let y = margin_top in
      (* Table *)
      let cellpadding = 0.5 in
      let padding = cellpadding, cellpadding, cellpadding, cellpadding in
      let print_title text =
        `Func begin fun ~x ~y ~width ->
          let markup = sprintf "<span align='0.5'>%s</span>" text in
          let width, height = PDFMarkup.print ~x ~y ~width ~markup (*~valign:0.5 *)~padding doc in
          height
        end;
      in
      let columns =
        let open PDFTable in [
          `GRANDEZZE,       {col_width = 15.5; col_title = print_title "GRANDEZZE"};
          `SIMBOLO,         {col_width = 7.5;  col_title = print_title "Simbolo\ne\nlegame dimensionale"};
          `DIMENSIONI,      {col_width = 7.;   col_title = `Func begin fun ~x ~y ~width ->
              let markup = sprintf "<span align='0.5' line_spacing='%f'>Dimensioni</span><span align='0.5' size='%f'>(1)</span>"
                  line_spacing (font_size -. 2.)
              in
              let width, height = PDFMarkup.print ~x ~y ~width ~markup (*~valign:0.5 *)~padding doc in
              height
            end};
          `G_DENOMINAZIONE, {col_width = 11.0; col_title = print_title "Denominazione"};
          `G_ABBREV,        {col_width = 7.5;  col_title = print_title "Abbreviazione"};
          `T_DENOMINAZIONE, {col_width = 11.0; col_title = print_title "Denominazione"};
          `T_ABBREV,        {col_width = 7.5;  col_title = print_title "Abbreviazione"};
          `C_DENOMINAZIONE, {col_width = 11.0; col_title = print_title "Denominazione"};
          `C_ABBREV,        {col_width = 7.5;  col_title = print_title "Abbreviazione"};
          `EQ_G,            {col_width = 8.;   col_title = print_title "Sist. Giorgi"};
          `EQ_C,            {col_width = 6.5;  col_title = print_title "Sist. C.G.S."};
        ]
      in
      let header_layout = let open PDFTable in [
        `Leaf `GRANDEZZE;
        `Leaf `SIMBOLO;
        `Leaf `DIMENSIONI;
        `Node {
          h_draw                = `Text "SISTEMA GIORGI (M.K.S.)";
          h_children            = [
            `Node {
              h_draw                = `Text "UNIT\192 FONDAMENTALI\nmetro, chilogrammo (massa), secondo";
              h_children            = [
                `Leaf `G_DENOMINAZIONE;
                `Leaf `G_ABBREV;
              ];
          }];
        };
        `Node {
          h_draw                = `Text "SISTEMA TECNICO";
          h_children            = [
            `Node {
              h_draw                = `Text "UNIT\192 FONDAMENTALI\nmetro, chilogrammo (peso), secondo";
              h_children            = [
                `Leaf `T_DENOMINAZIONE;
                `Leaf `T_ABBREV;
              ];
          }];
        };
        `Node {
          h_draw                = `Text "SISTEMA C.G.S.";
          h_children            = [
            `Node {
              h_draw                = `Text "UNIT\192 FONDAMENTALI\ncentimetro, grammo (massa), secondo";
              h_children            = [
                `Leaf `C_DENOMINAZIONE;
                `Leaf `C_ABBREV;
              ];
          }];
        };
        `Node {
          h_draw                = `Func begin fun ~x ~y ~width ->
              let markup = "\
<SPAN align='0.5'>EQUIVALENZA<BR/>\
tra le unit\224 del </SPAN><SPAN style='italic'>sist. Giorgi</SPAN><BR/>\
<SPAN align='0.5'> e le corrispondenti</SPAN><BR/>\
<SPAN align='0.5'> del </SPAN><SPAN style='italic' align='0.5'>sist. C.G.S.</SPAN>" in
              let width, height = PDFMarkup.print ~x ~y ~width ~markup (*~valign:0.5*) doc in
              height
          end;
          h_children            = [
            `Leaf `EQ_G;
            `Leaf `EQ_C;
          ]
        }
      ] in
      let rows = Array.to_list (Array.create 3 (Array.create (List.length columns) None)) in
      let cell_func ~index ~row ~col =
        let prop = {PDFTable.
          prop_text       = (match row col with None -> "" | Some x -> x);
          prop_align      = `Left;
          prop_font_style = [];
          prop_font_size  = None;
          prop_image      = None;
          prop_bg_color   = None;
          prop_fg_color   = None;
        } in
        PDFTable.Cell_properties prop
      in
      PDFTable.print ~x ~y
        ~caption:"TABELLA DELLE PRINCIPALI UNIT\192 DI MISURA MECCANICHE"
        ~width:width_avail
        ~page_height:height_avail
        ~line_spacing
        ~header_layout
        ~grid_lines:`Vertical
        ~border_width:`Thick
        ~cellpadding
        (*~rowspacing:5.*)
        ~columns
        ~rows
        ~page_break_func:begin fun () ->
          PDF.add_page doc;
          PDF.set ~x ~y:(PDF.y doc +. 2.) doc;
        end
        ~cell_func
        doc;*)
      (**  *)
      PDF.close_document doc;
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

let _ = main ()
