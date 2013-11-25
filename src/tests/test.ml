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
open Fpdf_table


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
che lascia dietro a s\xE9 mar s\xEC crudele;</SPAN> <SPAN color='#0000FF' size='7' rise='5'>3</SPAN>

<SPAN style='italic,bold'>e canter\xF2 di quel </SPAN><SPAN scale='30'>secondo regno</SPAN><SPAN style='italic,bold'>
dove l\xB4umano spirito si purga
e di salire al ciel diventa degno.</SPAN> <SPAN color='#0000FF' size='7' rise='5'>6</SPAN>

<SPAN family='Courier' size='10' scale='70'>Ma qui la morta poes\xEC resurga,
o sante Muse, poi che vostro sono;
e qui Cal\xEFop\xE8 alquanto surga,</SPAN> <SPAN color='#0000FF' size='7' rise='5'>9</SPAN>

<SPAN style='bold'>seguitando il mio canto con quel suono
di cui le Piche misere sentiro
lo colpo tal, che disperar perdono.</SPAN> <SPAN color='#0000FF' size='7' rise='5'>12</SPAN>

<SPAN bgcolor='#f0f0ff' color='#ff1010'>Dolce color d\xB4or\xEFental zaffiro,
che s\xB4accoglieva nel sereno aspetto
del mezzo, puro infino al primo giro,</SPAN> <SPAN color='#0000FF' size='7' rise='5'>15</SPAN>

<SPAN family='Helvetica' >a li occhi miei ricominci\xF2 diletto,
tosto ch\xB4io usci\xB4 fuor de l\xB4aura morta
che m\xB4avea contristati li occhi e \xB4l petto.</SPAN> <SPAN color='#0000FF' size='7' rise='5'>18</SPAN>";;

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
      let radius = 1., 2., 3., 5. in
      let doc = Fpdf.create ~outchan () in
      Fpdf_font.embed_font ~family:`CenturySchoolbook ~style:[] doc;
      Fpdf_font.embed_font ~family:`CenturySchoolbook ~style:[`Italic] doc;
      Fpdf_font.embed_font ~family:`CenturySchoolbook ~style:[`Bold] doc;
      Fpdf_font.embed_font ~family:`CenturySchoolbook ~style:[`Bold; `Italic] doc;
      Fpdf_font.embed_font ~family:`CMUSerif ~style:[] doc;
      Fpdf_font.embed_font ~family:`CMUSerif ~style:[`Bold] doc;
      Fpdf_font.embed_font ~family:`CMUSerif ~style:[`Italic] doc;
      Fpdf_font.embed_font ~family:`CMUSerif ~style:[`Bold; `Italic] doc;
      Fpdf_font.embed_font ~family:`CMUSerif_BoldNonextended ~style:[] doc;
      Fpdf_font.embed_font ~family:`CMUSansSerif ~style:[] doc;
      Fpdf_font.embed_font ~family:`CMUSansSerif ~style:[`Bold] doc;
      Fpdf_font.embed_font ~family:`CMUSansSerif ~style:[`Italic] doc;
      Fpdf_font.embed_font ~family:`CMUSansSerif ~style:[`Bold; `Italic] doc;
      Fpdf_font.embed_font ~family:`CMUSansSerif_DemiCondensed ~style:[] doc;

      Fpdf.set_display_mode `Fullwidth doc;
      (*Fpdf.set_compression false doc;*)

      let title = "ocamlfpdf test" in
      let margin = 20. in
      Fpdf.set_margins ~left:margin ~top:margin doc;
      Fpdf.set_title title doc;
      Fpdf.set_creator "ocamlfpdf" doc;
      Fpdf.set_subject "test" doc;
      Fpdf.set_creation_date "(D:20121023)" doc;
      let family = `Helvetica in
      let spacing = 1.0 in
      let padding = 1.0 in
      let height_header = 10. +. spacing in
      let width_avail = Fpdf.page_width doc -. margin *. 2. in
      let height_avail = Fpdf.page_height doc -. margin *. 2. -. height_header in
      let line_height = (Fpdf.font_size doc) /. Fpdf.scale doc +. 0.5 in

      (** Header *)
      Fpdf.set_header_func begin fun () ->
        Fpdf.set_font ~family:`Helvetica ~size:20. doc;
        let width_page_num = 50. in
        let height = height_header -. spacing in
        Fpdf.set_line_width 0.1 doc;
        Fpdf.rect ~width:width_avail ~height doc;
        Fpdf.set ~x:margin ~y:margin doc;
        Fpdf.set_font ~scale:82 doc;
        Fpdf.multi_cell ~width:(width_avail -. width_page_num) ~line_height:height ~text:title doc;
        (* Title and page number *)
        Fpdf.set_font ~family:`Helvetica ~size:10. doc;
        Fpdf.set ~x:(margin +. width_avail -. width_page_num) ~y:margin doc;
        Fpdf.multi_cell ~width:width_page_num ~line_height:height ~align:`Right
          ~text:(sprintf "%s - %d/{nb}" (Filename.basename filename) (Fpdf.page_count doc)) doc;
      end doc;

      (** Markup *)
      Fpdf.add_page doc;
      ignore (Fpdf_bookmark.add ~text:"Markup" doc);
      Fpdf.set_font ~family:`Times ~size:12. doc;
      let x = margin in
      let y = margin +. height_header (* *. 5. /. 3.*) in
      let width = (*50.*) width_avail (*/. 2.3*) in
      let height = height_avail in
      Fpdf.set_line_width 1.0 doc;
      Fpdf.rect ~x ~y ~width ~height ~style:`Outline doc;
      let width = width_avail *. 0.85 in
      let x = x +. (width_avail -. width) /. 2. in
      let markup = Fpdf_markup.prepare ~width ~padding:(30., 30., 30., 30.) ~markup
        ~bgcolor:"#fffff0" ~border_width:0.2 ~border_color:"#ff0000" ~border_radius:radius doc in
      markup.Fpdf_markup.print ~x ~y ~valign:(height_avail, 0.5) ();

      (** Markup and wrap char *)
      Fpdf.add_page doc;
      let width = width_avail in
      ignore (Fpdf_bookmark.add ~text:"Markup - Wrap (1)" doc);
      let x = margin in
      let y = margin +. height_header *. 5. /. 3. in
      let markup = "WRAP CHARRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR" in
      Fpdf.set_font ~family:`Times ~size:12. doc;
      let markup = Fpdf_markup.prepare ~width:(width/.2.) (*~line_height*) (*~padding:(5.,5.,5.,5.)*) ~markup
        ~bgcolor:"#fff0f0" ~border_width:5. ~border_color:"#f00000" (*~border_radius:3.*) doc in
      markup.Fpdf_markup.print ~x ~y ();
      (*  *)
      let y = y +. markup.Fpdf_markup.height +. 30. in
      let markup = "<SPAN align='0.5'>\128</SPAN><SPAN style='bold' size='55' bgcolor='#ffff00'>123</SPAN><SPAN style='bold' rise='15' bgcolor='#ffff00'>,</SPAN><SPAN style='bold' rise='15' underline='single' bgcolor='#ffff00'>99</SPAN><SPAN>Test</SPAN>" in
      Fpdf.set_font ~family:`Helvetica ~size:34. doc;
      let markup = Fpdf_markup.prepare ~width ~markup
        ~bgcolor:"#fff0f0" ~border_width:1. ~border_color:"#f00000" (*~border_radius:3.*) doc in
      markup.Fpdf_markup.print ~x ~y ();

      (** Lorem Ipsum *)
      Fpdf.add_page doc;
      ignore (Fpdf_bookmark.add ~text:"Markup" doc);
      let x = margin /. 2. in
      let y = margin +. height_header *. 5. /. 3. in
      Fpdf.set_font ~family:(*`Times*) `CMUSerif ~size:12. doc;
      Fpdf.set_fill_color ~red:255 ~green:200 ~blue:255 doc;
      let width = (*width_avail*) Fpdf.page_width doc -. margin in
      let markup = Fpdf_markup.prepare ~width ~markup:lorem_ipsum ~padding:(10.,10.,10.,10.)
        (*~bgcolor:"#fff0f0" ~border_width:0.5 ~border_color:"#f00000" ~border_radius:3.*) doc
      in
      markup.Fpdf_markup.print ~x ~y ();

      (** Graphics *)
      Fpdf.add_page doc;
      ignore (Fpdf_bookmark.add ~text:"Graphics" doc);
      let width = width_avail in
      let x = margin in
      let y = margin +. height_header +. 10. in
      Fpdf.set_line_width 1. doc;

      Fpdf.push_graphics_state doc;
      Fpdf.set_line_cap `Round doc;
      Fpdf.set_line_width 0.1 doc;
      Fpdf.set_line_dash [2; 2] ~phase:0 doc;
      Fpdf.line ~x1:x ~y1:y ~x2:(x +. width /. 2.) ~y2:y doc;
      Fpdf.set_line_dash [3; 5] ~phase:6 doc;

      let y = y +. 10. in
      Fpdf.line ~x1:x ~y1:y ~x2:(x +. width /. 2.) ~y2:y doc;
      let y = y +. 10. in
      Fpdf.rect ~x ~y ~width:(x +. width /. 2.) ~height:y doc;
      Fpdf.pop_graphics_state doc;

      let y = y +. 10. in
      Fpdf.set_line_join `Bevel doc;
      Fpdf.rect ~x:(x +. 3.) ~y ~width:(x +. width /. 3.) ~height:y doc;

      (** Vertical box *)
      Fpdf.add_page doc;
      ignore (Fpdf_bookmark.add ~text:"Vertical box" doc);
      let width = width_avail *. 0.5 in
      let x = margin +. (width_avail -. width) /. 2. in
      let y = margin +. height_header in
      let vbox = new Fpdf_pack.vbox ~x ~y ~width ~height:60. ~padding:0. ~spacing doc in
      Array.iteri begin fun i child ->
        vbox#add begin fun ~x ~y ~width ~height ->
          let child_height = height *. child in
          Fpdf.rect ~x ~y ~width ~height:child_height ~radius doc;
          Fpdf.set ~x ~y doc;
          Fpdf.multi_cell ~width ~line_height:child_height ~align:`Center
            ~text:(sprintf "Vertical box, child n. %d" (i + 1)) doc;
          child_height
        end;
      end [|0.15; 0.23; 0.62|];
      vbox#pack();

      (** Horizontal box *)
      Fpdf.add_page doc;
      ignore (Fpdf_bookmark.add ~text:"Horizontal box" doc);
      let width = width_avail in
      let height = 60. in
      let x = margin +. (width_avail -. width) /. 2. in
      let y = margin +. height_header in
      let hbox = new Fpdf_pack.hbox ~x ~y ~width ~height ~padding:0. ~spacing doc in
      Array.iteri begin fun i child ->
        hbox#add begin fun ~x ~y ~width ~height ->
          let child_width = width *. child in
          let child_inner_width = child_width -. padding *. 2. in
          Fpdf.rect ~x ~y ~width:child_width ~height ~radius doc;
          let text = sprintf "Horizontal box, child n. %d" (i + 1) in
          let font =
            match Fpdf_document.find_font ~family ~style:[] doc with
              | Some x -> x | _ -> assert false
          in
          let size = Fpdf_util.fixpoint begin fun size ->
            let text_width = Fpdf.get_text_width font.Fpdf_document.font_metrics size text in
            let text_width = text_width /. (Fpdf.scale doc) in
            if text_width < child_inner_width then size else (size -. 0.25)
          end 30. in
          Fpdf.set_font ~family ~size doc;
          Fpdf.set ~x ~y doc;
          let line_height = size /. Fpdf.scale doc +. 0.5 in
          Fpdf.multi_cell ~width:child_width ~padding ~line_height ~align:`Center ~text doc;
          child_width
        end;
      end [|0.15; 0.23; 0.62|];
      hbox#pack();

      (** Barcodes *)
      Fpdf.add_page doc;
      let parent = Fpdf_bookmark.add ~text:"Barcodes" doc in
      let barcode = "abc123456789" in
      let x = margin +. (width_avail -. width) /. 2. in
      let y = margin +. height_header in
      let height = 10. in
      ignore (Fpdf_bookmark.add ~text:"Code39" (*~level:1*) ~y ~parent doc);
      Fpdf_barcode.Code39.write ~x ~y ~barcode ~height ~baseline:1. ~text:10. doc;
      let y = y +. height +. 20. in
      ignore (Fpdf_bookmark.add ~text:"EAN13" (*~level:1*) ~y ~parent doc);
      Fpdf_barcode.EAN13.write ~x ~y ~barcode:"8711253001202" ~width:1.0 doc;
      let y = y +. height +. 20. in
      Fpdf_barcode.EAN13.write ~x ~y ~barcode:"3800065711135" doc;
      let y = y +. height +. 20. in
      Fpdf_barcode.EAN13.write ~x ~y ~barcode:"4556789034461" ~width:(0.33 *. 0.8) doc;
      let y = y +. height +. 20. in
      ignore (Fpdf_bookmark.add ~text:"Code128C" (*~level:1*) ~y ~parent doc);
      Fpdf_barcode.Code128C.write ~x ~y ~barcode:"123456" ~text:10. doc;

      (** Form fields *)
      Fpdf.add_page doc;
      let form = Fpdf_form.get doc in
      let x = margin in
      let y = 2. *. margin +. height_header in
      Fpdf.set ~x ~y doc;
      ignore (Fpdf_bookmark.add ~text:"Interactive Forms" doc);
      let field =
        Fpdf_form.add_text_field ~x ~y ~width:80. ~height:20.
          ~maxlength:30 ~readonly:false ~justification:`Center
          ~name:"test_field" ~value:"" ~default_value:"3" ~value_ap:"Simple text field"
          ~fgcolor_ap:"#909090" ~font_style:[`Italic]
          ~bgcolor:"#f0f0f0" ~border:(`Underline, "#000000") form
      in

      (** Images *)
      Fpdf.add_page doc;
      ignore (Fpdf_bookmark.add ~text:"Images" doc);
      let dirname = Filename.dirname Sys.executable_name in
      let name = "Lena.jpg" in
      let data = Fpdf_util.fread (dirname // name) in
      let image_width, image_height = Fpdf_images.Jpeg.get_dimensions data in
      let aspect = (float image_height) /. (float image_width) in
      let height_image = 50. in
      let width_image = height_image /. aspect in
      let x = margin +. (width_avail -. width) /. 2. in
      let y = margin +. height_header in
      Fpdf.image ~x ~y ~name ~data ~height:height_image doc;
      Fpdf.set ~x:(x +. width_image +. spacing) ~y doc;
      let text = Str.global_replace (Str.regexp "\\(  \\)\\|[\n]") ""
          ((Str.string_before (Fpdf_util.fread (dirname // "test.ml"))) 1000) in
      Fpdf.set_font ~family:`CMUSansSerif ~size:9. doc;
      Fpdf.multi_cell ~width:(width_avail -. width_image -. spacing) ~padding ~line_height ~align:`Left ~border:[] ~text doc;

      (** Fpdf_table *)
      Fpdf.add_page doc;
      let width = width_avail *. 0.7 in
      let x = margin +. (width_avail -. width) /. 2. in
      let y = margin +. height_header +. spacing +. 40. in
      Fpdf.set_font ~family:`CenturySchoolbook ~size:9. doc;
      ignore (Fpdf_bookmark.add ~text:"Fpdf_table Example" ~y:(y -. 10.) doc);
      (**  *)
      Fpdf_table.print ~x:10. ~y
        ~caption:""
        ~grid_lines:`Vertical
        ~width:25.
        ~page_height:height_avail
        ~columns:[
          `A, {Fpdf_table.col_width = 38.; col_title = `Text "A"};
          `B, {Fpdf_table.col_width = 62.; col_title = `Text "B"};
        ]
        ~rows:[
          [|Some "a"; Some "\128"|];
          [|Some "1"; Some "3"|];
          [|Some "A"; Some "B"|];
        ]
        doc;
      (**  *)
      let line_height = (Fpdf.font_size doc) /. Fpdf.scale doc +. 0.5 in
      let cell_func ~index ~row ~col =
        let prop = {Fpdf_table.
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
          Fpdf.set_fill_color ~red:255 ~green:212 ~blue:0 doc;
          Fpdf.rect ~x:x1 ~y:y1 ~width:overlap ~height:overlap ~style:`Fill doc;
          Fpdf.rect ~x ~y ~width:w1 ~height:h1 ~style:`Outline doc;
          let w2 = width *. 0.6 +. overlap in
          let h2 = height *. 0.5 +. overlap in
          Fpdf.rect ~x:x1 ~y:y1 ~width:w2 ~height:h2 ~style:`Outline doc;
          let text = match row col with None -> "" | Some x -> x in
          let markup = sprintf "<SPAN color='#ff0000' align='0.5'>%s</SPAN>" text in
          let w3 = w2 -. overlap in
          ignore (Fpdf_markup.print ~x:(x1 +. overlap) ~y:(y1 +. overlap) ~width:w3 ~markup doc)
        end) else if col = `B then Cell_properties {prop with Fpdf_table.
          prop_text       = (match row col with None -> "-" | Some x -> x);
          prop_align      = `Left;
          prop_font_style = prop.Fpdf_table.prop_font_style @ [`Italic];
        } else Cell_properties prop
      in
      let rows = Array.create 300 [|Some "Text"; Some "text"; Some "Text"; Some "a"; Some "a"; Some "a"; Some "a"; Some "a"|] in
      let rows = Array.to_list rows in
      let print_title text =
        `Func begin fun ~x ~y ~width ->
          Fpdf.set_font ~style:[`Bold] doc;
          Fpdf_text.multi_cell ~width ~line_height ~text ~align:`Center doc;
          Fpdf.set_font ~style:[] doc;
          Fpdf.y doc -. y +. padding
        end;
      in
      let columns = [
        `A, {Fpdf_table.col_width = 12.5; col_title = print_title "Column A"};
        `B, {Fpdf_table.col_width = 25.; col_title = print_title "Column B"};
        `C, {Fpdf_table.col_width = 12.5; col_title = print_title "Column C"};
        `D, {Fpdf_table.col_width = 6.25; col_title = print_title "Column D"};
        `E, {Fpdf_table.col_width = 12.5; col_title = print_title "Column E"};
        `F, {Fpdf_table.col_width = 12.5; col_title = print_title "Column F"};
        `G, {Fpdf_table.col_width = 12.5; col_title = print_title "Column G"};
        `H, {Fpdf_table.col_width = 6.25; col_title = print_title "Column H"};
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
      Fpdf_table.print ~x ~y
        ~caption:"Fpdf_table Example"
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
          Fpdf.add_page doc;
          Fpdf.set ~x ~y:(Fpdf.y doc +. 2.) doc;
        end
        ~cell_func
        doc;
      (**  *)
      let test_parent = Fpdf_bookmark.add ~text:"TEST" doc in
      let parent = Fpdf_bookmark.add ~text:"CHILD" ~parent:test_parent doc in
      ignore (Fpdf_bookmark.add ~parent ~text:"Test: à \128" doc);
      ignore (Fpdf_bookmark.add ~parent ~text:(Fpdf_util.utf8_to_utf16 "Test: à €") doc);
      let parent = Fpdf_bookmark.add ~text:"CHILD 2" ~parent doc in
      let parent = Fpdf_bookmark.add ~text:"CHILD 3" ~parent doc in
      let _ = Fpdf_bookmark.add ~text:"CHILD 4" ~parent doc in

      (* Include javascript *)
      (*Fpdf_javascript.set_autoprint ~dialog:true doc;*)

      (** Close PDF document *)
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
