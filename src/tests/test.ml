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
open FPDFTable


let (//) = Filename.concat

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

let markup = "<SPAN align='0.5' style='bold' size='16'>Purgatorio, CANTO I</SPAN><BR/><BR/><BR/>\
<SPAN>Per correr </SPAN><SPAN underline='single'>miglior</SPAN><SPAN> </SPAN><SPAN underline='low'>acque</SPAN><SPAN> alza le vele
omai la navicella del mio ingegno,
che lascia dietro a s\xE9 mar s\xEC crudele;</SPAN> <SPAN color='#0000FF' size='7'>3</SPAN>

<SPAN style='italic,bold'>e canter\xF2 di quel </SPAN><SPAN scale='30'>secondo regno</SPAN><SPAN style='italic,bold'>
dove l\xB4umano spirito si purga
e di salire al ciel diventa degno.</SPAN> <SPAN color='#0000FF' size='7'>6</SPAN>

<SPAN family='Courier' size='10' scale='70'>Ma qui la morta poes\xEC resurga,
o sante Muse, poi che vostro sono;
e qui Cal\xEFop\xE8 alquanto surga,</SPAN> <SPAN color='#0000FF' size='7'>9</SPAN>

<SPAN style='bold'>seguitando il mio canto con quel suono
di cui le Piche misere sentiro
lo colpo tal, che disperar perdono.</SPAN> <SPAN color='#0000FF' size='7'>12</SPAN>

<SPAN bgcolor='#f0f0ff' color='#ff1010'>Dolce color d\xB4or\xEFental zaffiro,
che s\xB4accoglieva nel sereno aspetto
del mezzo, puro infino al primo giro,</SPAN> <SPAN color='#0000FF' size='7'>15</SPAN>

<SPAN family='Helvetica' >a li occhi miei ricominci\xF2 diletto,
tosto ch\xB4io usci\xB4 fuor de l\xB4aura morta
che m\xB4avea contristati li occhi e \xB4l petto.</SPAN> <SPAN color='#0000FF' size='7'>18</SPAN>";;

let lorem_ipsum = "\
<SPAN size='30' align='0.5' style='bold'>Lorem ipsum</SPAN>

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum imperdiet \
diam id diam aliquam porta at ac urna. Quisque justo est, cursus ac rutrum sed, \
accumsan sed arcu. Maecenas at urna velit. Fusce sagittis ullamcorper urna vel \
commodo. <SPAN size='20'>Integer gravida feugiat</SPAN> eros nec vestibulum. Praesent quis gravida mi. \
Suspendisse eu magna enim. Cras quis augue sed purus posuere consequat. \

Mauris vestibulum tempus ipsum ac iaculis. Nunc lorem augue, semper eget gravida \
sed, placerat volutpat nunc. <SPAN scale='60' underline='single'>Quisque sed tortor diam, ut blandit justo.</SPAN> \
Pellentesque consectetur mauris tellus. Integer <SPAN size='6' underline='single'>pellentesque</SPAN> elit vel orci \
venenatis sollicitudin. Sed lacinia magna vitae ante elementum suscipit. \
Praesent faucibus aliquam eros, ac porta lectus rutrum sed. Nulla consequat \
velit ac risus congue scelerisque. Sed quis luctus dui. Donec ipsum nisi, \
posuere sit amet venenatis vel, varius quis nunc. <SPAN char_space='1.0' underline='single'>Phasellus interdum laoreet \
arcu sit amet bibendum</SPAN>. Donec tempus ligula pretium ipsum bibendum non cursus \
augue aliquam. \

Quisque purus risus, hendrerit eget venenatis eget, tempus ut nunc. Cum sociis \
natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. \
<SPAN family='CMUSerif_BoldNonextended'>Integer est leo, eleifend quis pellentesque ac, sagittis viverra nisi</SPAN>. Aenean \
fermentum tempus tristique. <SPAN style='bold'>Fusce nec placerat risus. Fusce convallis sapien \
ante</SPAN>. Aenean sit amet ipsum sed odio laoreet vulputate sit amet nec ipsum. \
<SPAN style='italic'>Pellentesque ultrices leo ut arcu sagittis sit amet consequat erat hendrerit</SPAN>. \
Duis a purus lorem. Integer iaculis sodales convallis. <SPAN style='bold,italic'>Nulla dui mi, condimentum \
at faucibus eget, tristique in metus</SPAN>. \

Nam ullamcorper faucibus turpis, sit amet pharetra eros accumsan sagittis. \
Suspendisse potenti. Praesent sit amet imperdiet nibh. Cum sociis natoque \
penatibus et magnis dis parturient montes, nascetur ridiculus mus. Pellentesque \
felis risus, commodo nec pretium at, hendrerit sed ante. Aliquam auctor egestas \
lorem sed vulputate. Nulla semper purus vitae nisl congue quis eleifend odio \
tempor. <SPAN char_space='1.0' line_spacing='1.5' underline='single'>Donec iaculis accumsan convallis. Fusce lacinia mattis est at interdum. \
Suspendisse potenti. In hac habitasse platea dictumst. Praesent consectetur, \
dolor ac pellentesque adipiscing, mauris libero bibendum mi, sit amet dictum \
lacus ante non velit</SPAN>. Nunc laoreet odio nec risus pellentesque a pharetra turpis\
malesuada. Maecenas suscipit molestie turpis, et fringilla orci mattis ut. \

Quisque imperdiet convallis cursus. Nulla interdum libero non sapien aliquet eu \
hendrerit lorem lacinia. Nunc porta faucibus nibh, eget fringilla mauris \
scelerisque in. Mauris faucibus malesuada nulla, vel convallis justo fermentum \
vel. Quisque id faucibus metus. Nam mollis mattis nulla a commodo. Proin nibh \
lorem, tincidunt quis vestibulum at, iaculis vitae nunc. \
"


let main () = begin
  let filename = Sys.argv.(0) ^ ".pdf" in
  let outchan = open_out_bin filename in
  let close_file () = close_out outchan in
  begin
    try
      let radius = 1.0 in
      let doc = FPDF.create ~outchan () in
      FPDFFont.embed_font ~family:`CenturySchoolbook ~style:[] doc;
      FPDFFont.embed_font ~family:`CenturySchoolbook ~style:[`Italic] doc;
      FPDFFont.embed_font ~family:`CenturySchoolbook ~style:[`Bold] doc;
      FPDFFont.embed_font ~family:`CenturySchoolbook ~style:[`Bold; `Italic] doc;
      FPDFFont.embed_font ~family:`CMUSerif ~style:[] doc;
      FPDFFont.embed_font ~family:`CMUSerif ~style:[`Bold] doc;
      FPDFFont.embed_font ~family:`CMUSerif ~style:[`Italic] doc;
      FPDFFont.embed_font ~family:`CMUSerif ~style:[`Bold; `Italic] doc;
      FPDFFont.embed_font ~family:`CMUSerif_BoldNonextended ~style:[] doc;
      FPDFFont.embed_font ~family:`CMUSansSerif ~style:[] doc;
      FPDFFont.embed_font ~family:`CMUSansSerif ~style:[`Bold] doc;
      FPDFFont.embed_font ~family:`CMUSansSerif ~style:[`Italic] doc;
      FPDFFont.embed_font ~family:`CMUSansSerif ~style:[`Bold; `Italic] doc;
      FPDFFont.embed_font ~family:`CMUSansSerif_DemiCondensed ~style:[] doc;

      FPDF.set_display_mode `Fullwidth doc;
      (*FPDF.set_compression false doc;*)

      let title = "ocamlfpdf test" in
      let margin = 20. in
      FPDF.set_margins ~left:margin ~top:margin doc;
      FPDF.set_title title doc;
      FPDF.set_creator "ocamlfpdf" doc;
      FPDF.set_subject "test" doc;
      FPDF.set_creation_date "(D:20121023)" doc;
      let family = `Helvetica in
      let spacing = 1.0 in
      let padding = 1.0 in
      let height_header = 10. +. spacing in
      let width_avail = FPDF.page_width doc -. margin *. 2. in
      let height_avail = FPDF.page_height doc -. margin *. 2. -. height_header in
      let line_height = (FPDF.font_size doc) /. FPDF.scale doc +. 0.5 in

      (** Header *)
      FPDF.set_header_func begin fun () ->
        FPDF.set_font ~family:`Helvetica ~size:20. doc;
        let width_page_num = 50. in
        let height = height_header -. spacing in
        FPDF.set_line_width 0.1 doc;
        FPDF.rect ~width:width_avail ~height doc;
        FPDF.set ~x:margin ~y:margin doc;
        FPDF.multi_cell ~width:(width_avail -. width_page_num) ~line_height:height ~text:title doc;
        (* Title and page number *)
        FPDF.set_font ~family:`Helvetica ~size:10. doc;
        FPDF.set ~x:(margin +. width_avail -. width_page_num) ~y:margin doc;
        FPDF.multi_cell ~width:width_page_num ~line_height:height ~align:`Right
          ~text:(sprintf "%s - %d/{nb}" (Filename.basename filename) (FPDF.page_count doc)) doc;
      end doc;

      (** Markup *)
      FPDF.add_page doc;
      ignore (FPDFBookmark.add ~text:"Markup" doc);
      FPDF.set_font ~family:`Times ~size:12. doc;
      let x = margin in
      let y = margin +. height_header (* *. 5. /. 3.*) in
      let width = (*50.*) width_avail (*/. 2.3*) in
      let height = height_avail in
      FPDF.set_line_width 1.0 doc;
      FPDF.rect ~x ~y ~width ~height ~style:`Outline doc;
      let width = width_avail *. 0.85 in
      let x = x +. (width_avail -. width) /. 2. in
      let markup = FPDFMarkup.prepare ~width ~padding:(30., 30., 30., 30.) ~markup
        ~bgcolor:"#fffff0" ~border_width:0.2 ~border_color:"#ff0000" ~border_radius:3. doc in
      markup.FPDFMarkup.print ~x ~y ~valign:(height_avail, 0.5) ();

      (** Markup and wrap char *)
      FPDF.add_page doc;
      let width = width_avail in
      ignore (FPDFBookmark.add ~text:"Markup - Wrap (1)" doc);
      let x = margin in
      let y = margin +. height_header *. 5. /. 3. in
      let markup = "WRAP CHARRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR" in
      FPDF.set_font ~family:`Times ~size:12. doc;
      let markup = FPDFMarkup.prepare ~width:(width/.2.) (*~line_height*) (*~padding:(5.,5.,5.,5.)*) ~markup
        ~bgcolor:"#fff0f0" ~border_width:5. ~border_color:"#f00000" (*~border_radius:3.*) doc in
      markup.FPDFMarkup.print ~x ~y ();

      (** Lorem Ipsum *)
      FPDF.add_page doc;
      ignore (FPDFBookmark.add ~text:"Markup" doc);
      let x = margin /. 2. in
      let y = margin +. height_header *. 5. /. 3. in
      FPDF.set_font ~family:(*`Times*) `CMUSerif ~size:12. doc;
      FPDF.set_fill_color ~red:255 ~green:200 ~blue:255 doc;
      let width = (*width_avail*) FPDF.page_width doc -. margin in
      let markup = FPDFMarkup.prepare ~width ~markup:lorem_ipsum ~padding:(10.,10.,10.,10.)
        (*~bgcolor:"#fff0f0" ~border_width:0.5 ~border_color:"#f00000" ~border_radius:3.*) doc
      in
      markup.FPDFMarkup.print ~x ~y ();

      (** Graphics *)
      FPDF.add_page doc;
      ignore (FPDFBookmark.add ~text:"Graphics" doc);
      let width = width_avail in
      let x = margin in
      let y = margin +. height_header +. 10. in
      FPDF.set_line_width 1. doc;

      FPDF.push_graphics_state doc;
      FPDF.set_line_cap `Round doc;
      FPDF.set_line_width 0.1 doc;
      FPDF.set_line_dash [2; 2] ~phase:0 doc;
      FPDF.line ~x1:x ~y1:y ~x2:(x +. width /. 2.) ~y2:y doc;
      FPDF.set_line_dash [3; 5] ~phase:6 doc;

      let y = y +. 10. in
      FPDF.line ~x1:x ~y1:y ~x2:(x +. width /. 2.) ~y2:y doc;
      let y = y +. 10. in
      FPDF.rect ~x ~y ~width:(x +. width /. 2.) ~height:y doc;
      FPDF.pop_graphics_state doc;

      let y = y +. 10. in
      FPDF.set_line_join `Bevel doc;
      FPDF.rect ~x:(x +. 3.) ~y ~width:(x +. width /. 3.) ~height:y doc;

      (** Vertical box *)
      FPDF.add_page doc;
      ignore (FPDFBookmark.add ~text:"Vertical box" doc);
      let width = width_avail *. 0.5 in
      let x = margin +. (width_avail -. width) /. 2. in
      let y = margin +. height_header in
      let vbox = new FPDFPack.vbox ~x ~y ~width ~height:60. ~padding:0. ~spacing doc in
      Array.iteri begin fun i child ->
        vbox#add begin fun ~x ~y ~width ~height ->
          let child_height = height *. child in
          FPDF.rect ~x ~y ~width ~height:child_height ~radius doc;
          FPDF.set ~x ~y doc;
          FPDF.multi_cell ~width ~line_height:child_height ~align:`Center
            ~text:(sprintf "Vertical box, child n. %d" (i + 1)) doc;
          child_height
        end;
      end [|0.15; 0.23; 0.62|];
      vbox#pack();

      (** Horizontal box *)
      FPDF.add_page doc;
      ignore (FPDFBookmark.add ~text:"Horizontal box" doc);
      let width = width_avail in
      let height = 60. in
      let x = margin +. (width_avail -. width) /. 2. in
      let y = margin +. height_header in
      let hbox = new FPDFPack.hbox ~x ~y ~width ~height ~padding:0. ~spacing doc in
      Array.iteri begin fun i child ->
        hbox#add begin fun ~x ~y ~width ~height ->
          let child_width = width *. child in
          let child_inner_width = child_width -. padding *. 2. in
          FPDF.rect ~x ~y ~width:child_width ~height ~radius doc;
          let text = sprintf "Horizontal box, child n. %d" (i + 1) in
          let font =
            match FPDFDocument.find_font ~family ~style:[] doc with
              | Some x -> x | _ -> assert false
          in
          let size = FPDFUtil.fixpoint begin fun size ->
            let text_width = FPDF.get_text_width font.FPDFDocument.font_metrics size text in
            let text_width = text_width /. (FPDF.scale doc) in
            if text_width < child_inner_width then size else (size -. 0.25)
          end 30. in
          FPDF.set_font ~family ~size doc;
          FPDF.set ~x ~y doc;
          let line_height = size /. FPDF.scale doc +. 0.5 in
          FPDF.multi_cell ~width:child_width ~padding ~line_height ~align:`Center ~text doc;
          child_width
        end;
      end [|0.15; 0.23; 0.62|];
      hbox#pack();

      (** Barcodes *)
      FPDF.add_page doc;
      let parent = FPDFBookmark.add ~text:"Barcodes" doc in
      let barcode = "abc1234" in
      let x = margin +. (width_avail -. width) /. 2. in
      let y = margin +. height_header in
      let height = 10. in
      ignore (FPDFBookmark.add ~text:"Code39" (*~level:1*) ~y ~parent doc);
      Barcode.Code39.write ~x ~y ~barcode ~height ~baseline:1. ~text:10. doc;
      let y = y +. height +. 20. in
      ignore (FPDFBookmark.add ~text:"EAN13" (*~level:1*) ~y ~parent doc);
      Barcode.EAN13.write ~x ~y ~barcode:"8711253001202" ~width:1.0 doc;
      let y = y +. height +. 20. in
      Barcode.EAN13.write ~x ~y ~barcode:"3800065711135" doc;
      let y = y +. height +. 20. in
      Barcode.EAN13.write ~x ~y ~barcode:"4556789034461" ~width:(0.33 *. 0.8) doc;
      let y = y +. height +. 20. in
      ignore (FPDFBookmark.add ~text:"Code128C" (*~level:1*) ~y ~parent doc);
      Barcode.Code128C.write ~x ~y ~barcode:"123456" doc;

      (** Form fields *)
      FPDF.add_page doc;
      let form = FPDFForm.get doc in
      let x = margin in
      let y = 2. *. margin +. height_header in
      FPDF.set ~x ~y doc;
      ignore (FPDFBookmark.add ~text:"Interactive Forms" doc);
      let field =
        FPDFForm.add_text_field ~x ~y ~width:80. ~height:20.
          ~maxlength:30 ~readonly:false ~justification:`Center
          ~name:"test_field" ~value:"" ~default_value:"3" ~value_ap:"Simple text field"
          ~fgcolor_ap:"#909090" ~font_style:[`Italic]
          ~bgcolor:"#f0f0f0" ~border:(`Underline, "#000000") form
      in

      (** Images *)
      FPDF.add_page doc;
      ignore (FPDFBookmark.add ~text:"Images" doc);
      let dirname = Filename.dirname Sys.executable_name in
      let name = "Lena.jpg" in
      let data = Buffer.contents (FPDFUtil.fread (dirname // name)) in
      let image_width, image_height = FPDFImages.Jpeg.get_dimensions data in
      let aspect = (float image_height) /. (float image_width) in
      let height_image = 50. in
      let width_image = height_image /. aspect in
      let x = margin +. (width_avail -. width) /. 2. in
      let y = margin +. height_header in
      FPDF.image ~x ~y ~name ~data ~height:height_image doc;
      FPDF.set ~x:(x +. width_image +. spacing) ~y doc;
      let text = Str.global_replace (Str.regexp "\\(  \\)\\|[\n]") ""
          (Str.string_before (Buffer.contents (FPDFUtil.fread (dirname // "test.ml"))) 1000) in
      FPDF.set_font ~family:`CMUSansSerif ~size:9. doc;
      FPDF.multi_cell ~width:(width_avail -. width_image -. spacing) ~padding ~line_height ~align:`Left ~border:[] ~text doc;

      (** FPDFTable *)
      FPDF.add_page doc;
      let width = width_avail *. 0.7 in
      let x = margin +. (width_avail -. width) /. 2. in
      let y = margin +. height_header +. spacing +. 40. in
      FPDF.set_font ~family:`CenturySchoolbook ~size:9. doc;
      ignore (FPDFBookmark.add ~text:"FPDFTable Example" ~y:(y -. 10.) doc);
      (**  *)
      FPDFTable.print ~x:10. ~y
        ~caption:""
        ~grid_lines:`Vertical
        ~width:25.
        ~page_height:height_avail
        ~columns:[
          `A, {FPDFTable.col_width = 38.; col_title = `Text "A"};
          `B, {FPDFTable.col_width = 62.; col_title = `Text "B"};
        ]
        ~rows:[
          [|Some "a"; Some "\128"|];
          [|Some "1"; Some "3"|];
          [|Some "A"; Some "B"|];
        ]
        doc;
      (**  *)
      let line_height = (FPDF.font_size doc) /. FPDF.scale doc +. 0.5 in
      let cell_func ~index ~row ~col =
        let prop = {FPDFTable.
          prop_text       = (match row col with None -> "" | Some x -> x);
          prop_align      = `Center;
          prop_font_style = (if is_fib index then [`Bold] else []);
          prop_font_size  = None;
          prop_image      = None;
          prop_bg_color   = (if is_fib index then Some (220, 220, 255) else None);
          prop_fg_color   = None;
        } in
        if col = `B && index = 3 then Cell_draw (10., begin fun ~x ~y ~width ~height ->
          let overlap = 2. in
          let w1 = width *. 0.4 in
          let h1 = height *. 0.5 in
          let x1 = x +. w1 -. overlap in
          let y1 = y +. h1 -. overlap in
          FPDF.set_fill_color ~red:255 ~green:212 ~blue:0 doc;
          FPDF.rect ~x:x1 ~y:y1 ~width:overlap ~height:overlap ~style:`Fill doc;
          FPDF.rect ~x ~y ~width:w1 ~height:h1 ~style:`Outline doc;
          let w2 = width *. 0.6 +. overlap in
          let h2 = height *. 0.5 +. overlap in
          FPDF.rect ~x:x1 ~y:y1 ~width:w2 ~height:h2 ~style:`Outline doc;
          let text = match row col with None -> "" | Some x -> x in
          let markup = sprintf "<SPAN color='#ff0000' align='0.5'>%s</SPAN>" text in
          let w3 = w2 -. overlap in
          ignore (FPDFMarkup.print ~x:(x1 +. overlap) ~y:(y1 +. overlap) ~width:w3 ~markup doc)
        end) else if col = `B then Cell_properties {prop with FPDFTable.
          prop_text       = (match row col with None -> "-" | Some x -> x);
          prop_align      = `Left;
          prop_font_style = prop.FPDFTable.prop_font_style @ [`Italic];
        } else Cell_properties prop
      in
      let rows = Array.create 300 [|Some "Text"; Some "text"; Some "Text"; Some "a"; Some "a"; Some "a"; Some "a"; Some "a"|] in
      let rows = Array.to_list rows in
      let print_title text =
        `Func begin fun ~x ~y ~width ->
          FPDF.set_font ~style:[`Bold] doc;
          FPDFText.multi_cell ~width ~line_height ~text ~align:`Center doc;
          FPDF.set_font ~style:[] doc;
          FPDF.y doc -. y +. padding
        end;
      in
      let columns = [
        `A, {FPDFTable.col_width = 12.5; col_title = print_title "Column A"};
        `B, {FPDFTable.col_width = 25.; col_title = print_title "Column B"};
        `C, {FPDFTable.col_width = 12.5; col_title = print_title "Column C"};
        `D, {FPDFTable.col_width = 6.25; col_title = print_title "Column D"};
        `E, {FPDFTable.col_width = 12.5; col_title = print_title "Column E"};
        `F, {FPDFTable.col_width = 12.5; col_title = print_title "Column F"};
        `G, {FPDFTable.col_width = 12.5; col_title = print_title "Column G"};
        `H, {FPDFTable.col_width = 6.25; col_title = print_title "Column H"};
      ] in
      let header_layout = [
        `Node {
          h_draw = print_title "ABC";
          h_children = [
            `Node {
               h_draw                = print_title "AB";
               h_children            = [`Leaf `A; `Leaf `B];
            };
            `Leaf `C;
          ]
        };
        `Node {
          h_draw = print_title "DEFG";
          h_children = [
            `Node {
              h_draw = print_title "DE";
              h_children            = [`Leaf `D; `Leaf `E];
            };
            `Node {
              h_draw = print_title "FGH";
              h_children = [
                `Node {
                  h_draw = print_title "FG";
                  h_children            = [`Leaf `F; `Leaf `G]
                };
                `Leaf `H
              ]
            }
          ]
        }
      ] in
      FPDFTable.print ~x ~y
        ~caption:"FPDFTable Example"
        ~width
        ~page_height:height_avail
        ~page_header_height:height_header
        ~header_layout
        ~grid_lines:`Vertical
        ~columns
        ~rows:([
          [|Some "a"; Some "M"; Some "\128"; Some "a"; Some "a"; Some "a"; Some "a"; Some "a"|];
          [|Some "1"; None; Some "3"; Some "a"; Some "a"; Some "a"; Some "a"; Some "a"|];
          [|Some "A"; None; Some "B"; Some "a"; Some "a"; Some "a"; Some "a"; Some "a"|];
        ] @ rows)
        ~page_break_func:begin fun () ->
          FPDF.add_page doc;
          FPDF.set ~x ~y:(FPDF.y doc +. 2.) doc;
        end
        ~cell_func
        doc;
      (**  *)
      let test_parent = FPDFBookmark.add ~text:"TEST" doc in
      let parent = FPDFBookmark.add ~text:"CHILD" ~parent:test_parent doc in
      ignore (FPDFBookmark.add ~parent ~text:"Test: à \128" doc);
      ignore (FPDFBookmark.add ~parent ~text:(FPDFUtil.utf8_to_utf16 "Test: à €") doc);
      let parent = FPDFBookmark.add ~text:"CHILD 2" ~parent doc in
      let parent = FPDFBookmark.add ~text:"CHILD 3" ~parent doc in
      let _ = FPDFBookmark.add ~text:"CHILD 4" ~parent doc in

      (* Include javascript *)
      (*FPDFJavascript.set_autoprint ~dialog:true doc;*)

      (** Close PDF document *)
      FPDF.close_document doc;
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

let _ = FPDFError.handle_error main ()
