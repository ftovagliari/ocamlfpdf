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
open PDFTable

let rec fixpoint f v =
  let v' = f v in
  if v = v' then v else fixpoint f v'

let rec fib = function
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)

let is_fib n =
  let rec f i =
    if i > 20 then false
    else if n = fib i then true
    else f (i + 1)
  in f 0

let main () = begin
  let filename = Sys.argv.(0) ^ ".pdf" in
  let outchan = open_out_bin filename in
  let close_file () = close_out outchan in
  begin
    try
      let radius = 1.0 in
      let doc = PDF.create ~outchan () in
      (*PDF.set_compression false doc;*)

      let title = "OCaml-FPDF Test" in
      let margin = 20. in
      PDF.set_margins ~left:margin ~top:margin doc;
      PDF.set_title title doc;
      let family = `Helvetica in
      let spacing = 1.0 in
      let padding = 1.0 in
      let height_header = 10. +. spacing in
      let width_avail = PDF.page_width doc -. margin *. 2. in
      let height_avail = PDF.page_height doc -. margin *. 2. -. height_header in

      (** Header *)
      PDF.set_header_func begin fun () ->
        PDF.set_font ~family:`Helvetica ~size:20. doc;
        let width_page_num = 50. in
        let height = height_header -. spacing in
        PDF.set_line_width 0.1 doc;
        PDF.rect ~width:width_avail ~height doc;
        PDF.set ~x:margin ~y:margin doc;
        PDF.multi_cell ~width:(width_avail -. width_page_num) ~line_height:height ~text:title doc;
        (* Title and page number *)
        PDF.set_font ~family:`Helvetica ~size:10. doc;
        PDF.set ~x:(margin +. width_avail -. width_page_num) ~y:margin doc;
        PDF.multi_cell ~width:width_page_num ~line_height:height ~align:`Right
          ~text:(sprintf "%s - %d/{nb}" (Filename.basename filename) (PDF.n_pages doc)) doc;
      end doc;

      (** Markup *)
      PDF.add_page doc;
      let test_parent = PDFBookmark.add ~text:"PARENT" doc in
      ignore (PDFBookmark.add ~text:"Markup" doc);
      PDF.set_font ~family:`Times ~size:12. doc;
      let x = margin in
      let y = margin +. height_header *. 5. /. 3. in
      let width = (*50.*) width_avail (*/. 2.3*) in

      let markup = "\
<span>Per correr </span><span underline='single' bgcolor='#ffff00' size='20'>miglior</span>&nbsp;<span underline='low' bgcolor='#ffff00'>acque</span> alza le vele
omai la navicella del mio ingegno,
che lascia dietro a s\xE9 mar s\xEC crudele; <SPAN color='#0000FF' size='7'>3</SPAN>

<SPAN style='italic,bold'>e canter\xF2 di quel secondo regno
dove l\xB4umano spirito si purga
e di salire al ciel diventa degno.</SPAN> <SPAN color='#0000FF' size='7'>6</SPAN>

<SPAN family='courier'>Ma qui la morta poes\xEC resurga,
o sante Muse, poi che vostro sono;
e qui Cal\xEFop\xE8 alquanto surga,</SPAN> <SPAN color='#0000FF' size='7'>9</SPAN>

<SPAN style='bold'>seguitando il mio canto con quel suono
di cui le Piche misere sentiro
lo colpo tal, che disperar perdono.</SPAN> <SPAN color='#0000FF' size='7'>12</SPAN>

<SPAN bgcolor='#f0f0ff' color='#ff1010'>Dolce color d\xB4or\xEFental zaffiro,
che s\xB4accoglieva nel sereno aspetto
del mezzo, puro infino al primo giro,</SPAN> <SPAN color='#0000FF' size='7'>15</SPAN>

<SPAN family='helvetica' >a li occhi miei ricominci\xF2 diletto,
tosto ch\xB4io usci\xB4 fuor de l\xB4aura morta
che m\xB4avea contristati li occhi e \xB4l petto.</SPAN> <SPAN color='#0000FF' size='7'>18</SPAN>"
      in

      let _, _ = PDFMarkup.print ~x ~y ~width ~padding:3. ~markup
        ~bgcolor:"#fffff0" ~border_width:0.2 ~border_color:"#fff000" ~border_radius:3. doc in

      (** Markup and wrap char *)
      PDF.add_page doc;
      let x = margin in
      let y = margin +. height_header *. 5. /. 3. in
      let markup = "WRAP CHARRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR" in
      PDF.set_font ~family:`Times ~size:12. doc;
      let _, _ = PDFMarkup.print ~x ~y ~width:(width/.2.) (*~line_height*) ~padding:5. ~markup
        ~bgcolor:"#fff0f0" ~border_width:5. ~border_color:"#f00000" (*~border_radius:3.*) doc in

      (** Markup and wrap char *)
      PDF.add_page doc;
      ignore (PDFBookmark.add ~text:"Markup - Wrap" doc);
      let x = margin in
      let y = margin +. height_header *. 5. /. 3. in
      let markup = "\
<SPAN size='30'>The Objective </SPAN><SPAN size='30' style='bold' color='#ff0000' underline='single' align='0.5'>Caml</SPAN><SPAN size='30'> language</SPAN>


Foreword

This document is intended as a reference manual for the <SPAN size='30'>Objective Caml language</SPAN>. \
It lists the language constructs, and gives their precise syntax and informal semantics. \
It is by         no means a tutorial introduction to the language: there is not a <SPAN size='6'>single</SPAN> example. \
A good working knowledge of Caml is assumed.

No attempt has been made at mathematical rigor: words are employed with their intuitive \
meaning, without further definition. As a <SPAN size='30.' line_spacing='1.5'>consequence</SPAN>, the <SPAN underline='single'>typing</SPAN> rules have been left \
out, by   lack of the mathematical framework <SPAN underline='low'>required</SPAN> to express them, while they are \
definitely part of a full formal definition of the language.

Notations

The syntax of the language is given in BNF-like notation. Terminal symbols are set \
in typewriter font (like this). Non-terminal symbols are set in italic font (like that). \
Square brackets […] denote optional components. Curly brackets {…} denotes zero, one or \
several repetitions of the enclosed components. Curly bracket with a trailing plus sign {…}+ \
denote one or several repetitions of the enclosed components. Parentheses (…) denote grouping." in

(*      let markup = "The_syntax_of_the_language is given .Mtation__Terminal <SPAN \
style='bold'>aa</SPAN>              symbols_are_set_in_typewriter_font (like this). \
Non-terminal are set in_italic fosssssssnt font (like that)." in

      let markup = "The_syntax_of_the_language <SPAN style='bold'>is</SPAN> given <SPAN style='bold'>notation__Terminal</SPAN> \
aa symbols_are_set_in_typewriter_font (like this).  \
Non-terminal are set in_italic <SPAN style='bold' align='0.5'>fosssssssnt</SPAN> font (like that)." in*)

      PDF.set_font ~family:`Times ~size:12. doc;
      PDF.set_fill_color ~red:255 ~green:200 ~blue:255 doc;
      let width = (*50.*) width_avail /. 5. *. 3. +. 0.5 in
      let _, _ = PDFMarkup.print ~x ~y ~width ~markup
        ~bgcolor:"#fff0f0" (*~padding:2. ~border_width:0.5 ~border_color:"#f00000" ~border_radius:3.*) doc in

      (** Graphics *)
      PDF.add_page doc;
      ignore (PDFBookmark.add ~text:"Graphics" doc);
      let width = width_avail in
      let x = margin in
      let y = margin +. height_header +. 10. in
      PDF.set_line_width 1. doc;

      PDF.push_graphics_state doc;
      PDF.set_line_cap `Round doc;
      PDF.set_line_width 0.1 doc;
      PDF.set_line_dash [2; 2] ~phase:0 doc;
      PDF.line ~x1:x ~y1:y ~x2:(x +. width /. 2.) ~y2:y doc;
      PDF.set_line_dash [3; 5] ~phase:6 doc;

      let y = y +. 10. in
      PDF.line ~x1:x ~y1:y ~x2:(x +. width /. 2.) ~y2:y doc;
      let y = y +. 10. in
      PDF.rect ~x ~y ~width:(x +. width /. 2.) ~height:y doc;
      PDF.pop_graphics_state doc;

      let y = y +. 10. in
      PDF.set_line_join `Bevel doc;
      PDF.rect ~x:(x +. 3.) ~y ~width:(x +. width /. 3.) ~height:y doc;

      (** Vertical box *)
      PDF.add_page doc;
      ignore (PDFBookmark.add ~text:"Vertical box" doc);
      let width = width_avail *. 0.5 in
      let x = margin +. (width_avail -. width) /. 2. in
      let y = margin +. height_header in
      let vbox = new PDFPack.vbox ~x ~y ~width ~height:60. ~padding:0. ~spacing doc in
      Array.iteri begin fun i child ->
        vbox#add begin fun ~x ~y ~width ~height ->
          let child_height = height *. child in
          PDF.rect ~x ~y ~width ~height:child_height ~radius doc;
          PDF.set ~x ~y doc;
          PDF.multi_cell ~width ~line_height:child_height ~align:`Center
            ~text:(sprintf "Vertical box, child n. %d" (i + 1)) doc;
          child_height
        end;
      end [|0.15; 0.23; 0.62|];
      vbox#pack();

      (** Horizontal box *)
      PDF.add_page doc;
      ignore (PDFBookmark.add ~text:"Horizontal box" doc);
      let width = width_avail in
      let height = 60. in
      let x = margin +. (width_avail -. width) /. 2. in
      let y = margin +. height_header in
      let hbox = new PDFPack.hbox ~x ~y ~width ~height ~padding:0. ~spacing doc in
      Array.iteri begin fun i child ->
        hbox#add begin fun ~x ~y ~width ~height ->
          let child_width = width *. child in
          let child_inner_width = child_width -. padding *. 2. in
          PDF.rect ~x ~y ~width:child_width ~height ~radius doc;
          let text = sprintf "Horizontal box, child n. %d" (i + 1) in
          let size = fixpoint begin fun size ->
            PDF.set_font ~family ~size doc;
            let text_width = PDF.get_string_width text doc in
            if text_width < child_inner_width then size else (size -. 0.25)
          end 30. in
          PDF.set ~x ~y doc;
          let line_height = size /. PDF.scale doc +. 0.5 in
          PDF.multi_cell ~width:child_width ~padding ~line_height ~align:`Center ~text doc;
          child_width
        end;
      end [|0.15; 0.23; 0.62|];
      hbox#pack();

      (** Barcodes *)
      PDF.add_page doc;
      let parent = PDFBookmark.add ~text:"Barcodes" doc in
      let barcode = "abc1234" in
      let x = margin +. (width_avail -. width) /. 2. in
      let y = margin +. height_header in
      let height = 10. in
      ignore (PDFBookmark.add ~text:"Code39" (*~level:1*) ~y ~parent doc);
      Barcode.Code39.write ~x ~y ~barcode ~height ~baseline:1. ~text:10. doc;
      let y = y +. height +. 20. in
      ignore (PDFBookmark.add ~text:"EAN13" (*~level:1*) ~y ~parent doc);
      Barcode.EAN13.write ~x ~y ~barcode:"8711253001202" ~width:1.0 doc;
      let y = y +. height +. 20. in
      Barcode.EAN13.write ~x ~y ~barcode:"3800065711135" doc;
      let y = y +. height +. 20. in
      Barcode.EAN13.write ~x ~y ~barcode:"4556789034461" ~width:(0.33 *. 0.8) doc;
      let y = y +. height +. 20. in
      ignore (PDFBookmark.add ~text:"Code128C" (*~level:1*) ~y ~parent doc);
      Barcode.Code128C.write ~x ~y ~barcode:"123456" doc;

      (** Images *)
      PDF.add_page doc;
      ignore (PDFBookmark.add ~text:"Images" doc);
      let name = "Lena.jpg" in
      let data = Buffer.contents (PDFUtil.fread name) in
      let image_width = 220 in
      let image_height = 220 in
      let aspect = (float image_height) /. (float image_width) in
      let height_image = 50. in
      let width_image = height_image /. aspect in
      let x = margin +. (width_avail -. width) /. 2. in
      let y = margin +. height_header in
      PDF.image ~x ~y ~name ~data ~height:height_image ~image_width ~image_height doc;
      PDF.set ~x:(x +. width_image +. spacing) ~y doc;
      let text = Str.global_replace (Str.regexp "\\(  \\)\\|[\n]") ""
        (Str.string_before (Buffer.contents (PDFUtil.fread "test.ml")) 1000) in
      PDF.set_font ~family ~size:9. doc;
      let line_height = (PDF.font_size doc) /. PDF.scale doc +. 0.5 in
      PDF.multi_cell ~width:(width_avail -. width_image -. spacing) ~padding ~line_height ~align:`Left ~border:[] ~text doc;

      (** PDFTable *)
      PDF.add_page doc;
      ignore (PDFBookmark.add ~text:"PDFTable" doc);
      let width = width_avail *. 0.7 in
      let x = margin +. (width_avail -. width) /. 2. in
      let y = margin +. height_header +. spacing +. 40. in
      (**  *)
      PDFTable.print ~x:1. ~y
        ~caption:""
        ~grid_lines:`Vertical
        ~width:25.
        ~page_height:height_avail
        ~line_height
        ~columns:[
          `A, {PDFTable.col_width = 38.; col_title = "A"};
          `B, {PDFTable.col_width = 62.; col_title = "B"};
        ]
        ~rows:[
          [|Some "a"; Some "\128"|];
          [|Some "1"; Some "3"|];
          [|Some "A"; Some "B"|];
        ]
        doc;
      (**  *)
      PDF.set_font ~family ~size:9. doc;
      let line_height = (PDF.font_size doc) /. PDF.scale doc +. 0.5 in
      let cell_func ~index ~row ~col =
        let prop = {PDFTable.
          prop_text       = (match row col with None -> "" | Some x -> x);
          prop_align      = `Center;
          prop_font_style = (if is_fib index then [`Bold] else []);
          prop_font_size  = None;
          prop_image      = None;
          prop_bg_color   = (if is_fib index then Some (220, 220, 255) else None);
          prop_fg_color   = None;
        } in
        if col = `B then {prop with PDFTable.
          prop_text       = (match row col with None -> "-" | Some x -> x);
          prop_align      = `Left;
          prop_font_style = prop.PDFTable.prop_font_style @ [`Italic];
        } else prop
      in
      let rows = Array.create 300 [|Some "Text"; Some "text"; Some "Text"; Some "a"; Some "a"; Some "a"; Some "a"; Some "a"|] in
      let rows = Array.to_list rows in
      let w = (float (truncate (100. /. 8. *. 1000.))) /. 1000. in
      let columns = [
        `A, {PDFTable.col_width = w; col_title = "Column A"};
        `B, {PDFTable.col_width = w; col_title = "Column B"};
        `C, {PDFTable.col_width = w; col_title = "Column C"};
        `D, {PDFTable.col_width = w; col_title = "Column D"};
        `E, {PDFTable.col_width = w; col_title = "Column E"};
        `F, {PDFTable.col_width = w; col_title = "Column F"};
        `G, {PDFTable.col_width = w; col_title = "Column G"};
        `H, {PDFTable.col_width = w; col_title = "Column H"};
      ] in
      let header_layout = [
        `Node {
          h_draw = `Text "ABC";
          h_vertical_line_width = `Thick;
          h_children = [
            `Node {
               h_draw                = `Text "AB";
               h_vertical_line_width = `Thick;
               h_children            = [`Leaf `A; `Leaf `B];
            };
            `Leaf `C;
          ]
        };
        `Node {
          h_draw = `Text "DEFG";
          h_vertical_line_width = `Thin;
          h_children = [
            `Node {
              h_draw = `Text "DE";
              h_vertical_line_width = `Thin;
              h_children            = [`Leaf `D; `Leaf `E];
            };
            `Node {
              h_draw = `Text "FGH";
              h_vertical_line_width = `Thin;
              h_children = [
                `Node {
                  h_draw = `Text "FG";
                  h_vertical_line_width = `Thin;
                  h_children            = [`Leaf `F; `Leaf `G]
                };
                `Leaf `H
              ]
            }
          ]
        }
      ] in
      PDFTable.print ~x ~y
        ~caption:"PDFTable"
        ~width
        ~page_height:height_avail
        ~page_header_height:height_header
        ~line_height
        ~header_layout
        ~grid_lines:`Vertical
        (*~cellpadding:0.*)
        ~columns
        ~rows:([
          [|Some "a"; Some "M"; Some "\128"; Some "a"; Some "a"; Some "a"; Some "a"; Some "a"|];
          [|Some "1"; None; Some "3"; Some "a"; Some "a"; Some "a"; Some "a"; Some "a"|];
          [|Some "A"; None; Some "B"; Some "a"; Some "a"; Some "a"; Some "a"; Some "a"|];
        ] @ rows)
        ~page_break_func:begin fun () ->
          PDF.add_page doc;
          PDF.set ~x ~y:(PDF.y doc +. 2.) doc;
        end
        (*~cell_func*)
        doc;
      (**  *)
      let parent = PDFBookmark.add ~text:"CHILD" ~parent:test_parent doc in
      ignore (PDFBookmark.add ~parent ~text:"Test: à \128" doc);
      ignore (PDFBookmark.add ~parent ~text:(PDFUtil.utf8_to_utf16 "Test: à €") doc);
      let parent = PDFBookmark.add ~text:"CHILD 2" ~parent doc in
      let parent = PDFBookmark.add ~text:"CHILD 3" ~parent doc in
      let _ = PDFBookmark.add ~text:"CHILD 4" ~parent doc in

      (* Include javascript *)
      (*PDFJavascript.set_autoprint ~dialog:true doc;*)

      (** Close PDF document *)
      PDF.close_document doc;
      close_file();
    with ex -> begin
      close_file();
      raise ex
    end
  end;
  if Sys.os_type = "Win32" then ignore (Sys.command filename);
end

let _ = main ()