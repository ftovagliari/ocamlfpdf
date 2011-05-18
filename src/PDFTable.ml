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

type cell_properties = {
  mutable prop_text       : string;
  mutable prop_align      : PDF.align;
  mutable prop_font_style : Font.style list;
  mutable prop_font_size  : float option;
  mutable prop_bg_color   : (int * int * int) option;
  mutable prop_fg_color   : (int * int * int) option;
  prop_image              : image option
}

and image = {
  img_name   : string;
  img_data   : string;
  img_width  : int;
  img_height : int
}

and column = {
  mutable col_width : float; (* Percent *)
  col_title : string;
}

type 'a column_id = 'a

let rec list_pos ?(pos=0) ll x = match ll with
  | [] -> raise Not_found
  | a :: b -> if a = x then pos else 1 + (list_pos ~pos b x)

(** Table with automatic page break.
  * @param x Position of the left border of the table.
  * @param y Position of the top border of the table.
  * @param width Total width.
  * @param page_height Available height of the page body.
  ° @param line_height Height of the line of text.
  * @param columns
  * @param rows Cell contents.
  * @param cell_func
  * @param doc A [PDF] document.
  *)
let print
    ~x
    ~y
    ~width
    ~page_height
    ~line_height
    ~columns
    ~rows
    ?caption
    ?(cell_func=(fun ~index ~row ~col -> {
      prop_text = (match row col with None -> "" | Some x -> x);
      prop_align = `Center;
      prop_font_style = [];
      prop_font_size = None;
      prop_image = None;
      prop_bg_color = None;
      prop_fg_color = None;
    })) doc =
  let set_default_draw_color doc = PDF.set_draw_color ~red:0 ~green:0 ~blue:0 doc in
  let margin_top, _, margin_bottom, _ = PDF.margins doc in
  let add_page () =
    PDF.add_page doc;
    PDF.set_line_width 0.1 doc;
  in
  let perc =
    let sum_perc = ref 0.0 in fun x ->
      if x <= 0.0 then 0.0 else
        let w = if !sum_perc +. x > 100. then 100. -. !sum_perc else width *. x /. 100. in
        sum_perc := !sum_perc +. x;
        w
  in
  List.iter (fun (_, col) -> col.col_width <- (perc col.col_width)) columns;
  let x0 = x in
  let y0 = y in
  let x = ref x0 in
  let y = ref y0 in
  (* Titles of the columns *)
  let print_title ~x ~y ?align (_, col) =
    let width = col.col_width in
    if width > 0. then begin
      let text = col.col_title in
      PDF.set ~x:!x ~y:!y doc;
      PDF.multi_cell ~width ~line_height ~border:[] ~padding:0.5 ?align ~text doc;
      x := !x +. width;
    end;
    PDF.y doc
  in
  let space = 0.5 in
  let print_titles ~x ~y () =
    let x0, y0 = !x, !y in
    PDF.set_font ~style:[] doc;
    y := List.fold_left max 0.0 (List.map (print_title ~align:`Center ~x ~y) columns);
    (* *)
    PDF.line ~x1:x0 ~y1:y0 ~x2:(x0 +. width) ~y2:y0 doc;
    PDF.line ~x1:x0 ~y1:!y ~x2:!x ~y2:!y doc;
    y := !y +. space;
    PDF.line ~x1:x0 ~y1:!y ~x2:!x ~y2:!y doc;
    PDF.set_font ~style:[] doc;
    PDF.set ~x:!x ~y:!y doc;
  in
  (* Caption *)
  begin
    match caption with None -> () | Some caption ->
      PDF.set_font ~style:[`Bold] doc;
      PDF.set ~x:!x ~y:!y doc;
      PDF.multi_cell ~width ~line_height ~border:[] ~padding:0.5 ~align:`Center ~text:caption doc;
      y := !y +. line_height +. space;
  end;
  let top, left = ref !y, ref x0 in
  print_titles ~x ~y ();
  (* Vertical lines between columns *)
  let print_vertical_lines ~left ~top ~bottom () =
    let x = ref left in
    PDF.line ~x1:!x ~y1:top ~x2:!x ~y2:bottom doc;
    let print_line (_, col) =
      let width = col.col_width in
      if width > 0. then begin
        x := !x +. width;
        PDF.line ~x1:!x ~y1:top ~x2:!x ~y2:bottom doc
      end in
    List.iter print_line columns;
    PDF.line ~x1:left ~y1:bottom ~x2:(left +. width) ~y2:bottom doc;
  in
  (* Print rows *)
  let tags = fst (List.split columns) in
  let (!!) = PDFUtil.memo ~f:(list_pos tags) in
  let index = ref 0 in
  let cont = ref [] in
  List.iter begin fun row ->
    x := !left;
    let cell_props = Array.create (Array.length row) {
      prop_text = "";
      prop_align = `Left;
      prop_font_style = [];
      prop_font_size = None;
      prop_image = None;
      prop_bg_color = None;
      prop_fg_color = None;
    } in
    let i = ref 0 in
    let heights = List.map begin fun (tag, col) ->
      let row tag = row.(!! tag) in
      let cell = cell_func ~index:!index ~row ~col:tag in
      let maxh = match cell.prop_image with
        | None ->
          let text_width = PDF.get_string_width cell.prop_text doc in (* Works only with regular font *)
          let line_height = match cell.prop_font_size with None -> line_height | Some x -> PDF.font_size doc /. 2.5 in
          if col.col_width > 0.0 then (ceil (text_width /. col.col_width)) *. line_height else 0.0
        | Some image ->
          let asp = (float image.img_width) /. (float image.img_height) in
          if col.col_width > 0.0 then col.col_width /. asp else 0.0
      in
      cell_props.(!i) <- cell;
      incr i;
      maxh
    end columns in
    let max_height = List.fold_left max 0.0 heights in
    (* Salto pagina se necessario. Se una riga sconfina su più righe e l'altezza totale
     * supera lo spazio restante a fine pagina allora viene forzato un salto pagina (invece
     * di spezzare la riga). *)
    if !y -. margin_top +. max_height > page_height then begin
      List.iter (fun f -> f ()) !cont;
      cont := [];
      print_vertical_lines ~left:!left ~top:!top ~bottom:!y ();
      add_page();
      PDF.set ~x:x0 doc;
      left := PDF.x doc;
      top := PDF.y doc;
      x := PDF.x doc;
      y := PDF.y doc;
      print_titles ~x ~y ();
      x := !left;
    end;
    let ys = List.map begin fun (tag, col) ->
      let width = col.col_width in
      if width > 0. then begin
        let cell = cell_props.(!! tag) in
        let old_size = PDF.font_size doc in
        let old_style = PDF.font_style doc in
        PDF.set_font ~style:cell.prop_font_style ?size:cell.prop_font_size doc;
        PDF.set ~x:!x ~y:!y doc;
        let line_height = match cell.prop_font_size with None -> line_height | Some x -> PDF.font_size doc /. 2.5 in
        begin
          match cell.prop_image with
            | None ->
              let x = PDF.x doc in
              let y = PDF.y doc in
              begin
                match cell.prop_bg_color with None -> ()
                | Some (red, green, blue) ->
                  PDF.set_fill_color ~red ~green ~blue doc;
                  PDF.rect ~x ~y(*:(PDF.y doc +. PDF.line_width doc /. 2.)*) ~width
                    ~height:(line_height (*-. PDF.line_width doc*)) ~style:`Fill doc;
              end;
              PDF.multi_cell ~width ~line_height ~padding:0.5 ~align:cell.prop_align ~text:cell.prop_text doc;
            | Some image ->
              let asp = (float image.img_width) /. (float image.img_height) in
              PDF.image
                ~name:image.img_name
                ~data:image.img_data
                ~image_width:image.img_width
                ~image_height:image.img_height
                ~x:!x ~y:!y ~width doc;
              PDF.set ~y:(PDF.y doc +. width /. asp) doc
        end;
        PDF.set_font ~style:old_style ~size:old_size doc;
        x := !x +. width;
      end;
      PDF.y doc
    end columns in
    y := List.fold_left max 0.0 ys;
    (* Horizontal line between rows. *)
(*    PDF.set_draw_color ~red:200 ~green:200 ~blue:200 doc;*)
    let x1, y1, x2, y2 = !left, !y, !x, !y in
    let f = fun () -> PDF.line ~x1 ~y1 ~x2 ~y2 doc in
    cont := f :: !cont;
    set_default_draw_color doc;
    incr index;
  end rows;
  List.iter (fun f -> f ()) !cont;
  print_vertical_lines ~left:!left ~top:!top ~bottom:!y ()







