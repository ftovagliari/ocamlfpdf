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
open PDFTypes

type cell_properties = {
  mutable prop_text       : string;
  mutable prop_align      : align;
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

and cell_func =
  | Cell_properties of cell_properties
  | Cell_draw of float * (x:float -> y:float -> width:float -> height:float -> unit)

and column = {
  mutable col_width : float; (* Percent *)
  mutable col_title : header_draw_func;
}

and 'a column_id = 'a

and 'a tree = [`Node of 'a node | `Leaf of 'a column_id ]

and 'a node = {
  h_draw     : header_draw_func;
  h_children : 'a tree list;
}

and header_draw_func = [`Text of string | `Func of (x:float -> y:float -> width:float -> float)]

and thickness = [`Thin | `Medium | `Thick ]

let line_disjoin = 0.5

let size_of_thickness = function `Thin -> 0.1 | `Medium -> 0.25 | `Thick -> 0.5

let header_columns columns nodes =
  let rec f = function
    | `Leaf id -> (try [List.assoc id columns] with Not_found -> invalid_arg "header_columns")
    | `Node node -> List.flatten (List.map f node.h_children)
  in f nodes;;

let header_width col_group =
    (List.fold_left (fun acc c -> acc +. c.col_width) 0.0) col_group;;

let with_line_width doc lw f x =
  let old = PDF.line_width doc in
  PDF.set_line_width lw doc;
  f x;
  PDF.set_line_width old doc;;

let header_draw ~columns ~nodes ~x ~y ~padding ?align ~line_disjoin ~border_width doc =
  let y0 = y in
  let x = ref x in
  let border = [] in
  let points = ref [] in (* starting points for vertical lines *)
  let line_height = (PDF.font_size doc) /. PDF.scale doc in
  let rec draw_node level x y = function
    | `Leaf id ->
      let col = try List.assoc id columns with Not_found -> failwith "header_draw" in
      if col.col_width > 0. then begin
        let width = col.col_width -. 2. *. padding in
        PDF.set ~x:(x +. padding) ~y:(y +. padding) doc;
        let height =
          match col.col_title with
            | `Text text ->
              PDF.multi_cell ~width ~line_height ~border ?align ~text doc;
              PDF.y doc -. y +. padding;
            | `Func f -> (f ~x:(x +. padding) ~y:(y +. padding) ~width) +. padding;
        in
        points := (x +. col.col_width, y,  size_of_thickness (if level = 0 then `Medium else `Thin)) :: !points;
        width +. 2. *. padding, height
      end else 0., 0.
    | `Node node ->
      let widths = List.map header_width (List.map (header_columns columns) node.h_children) in
      let width = List.fold_left (+.) 0.0 widths in
      points := (x +. width, y, size_of_thickness (if level = 0 then `Medium else `Thin) (*node.h_vertical_line_width*)) :: !points;
      let width = width -. 2. *. padding in
      PDF.set ~x:(x +. padding) ~y:(y +. padding) doc;
      let height =
        match node.h_draw with
          | `Text text ->
            PDF.multi_cell ~width ~line_height ~border ?align ~text doc;
            PDF.y doc -. y +. padding
          | `Func f -> (f ~x:(x +. padding) ~y:(y +. padding) ~width) +. padding;
      in
      (*PDF.set_fill_color ~red:255 ~green:200 ~blue:255 doc;
      PDF.rect ~x ~y ~width ~height ~style:`Both doc;*)
      let x = ref x in
      let y = y +. height in
      PDF.line ~x1:(!x +. line_disjoin) ~y1:y ~x2:(!x +. width +. 2. *. padding -. line_disjoin) ~y2:y doc;
      let widths_heights = List.map begin fun node ->
        let width, height = draw_node (level + 1) !x y node in
        x := !x +. width;
        width, height
      end node.h_children in
      let _, heights = List.split widths_heights in
      let height = height +. (List.fold_left max 0.0 heights) in
      width +. 2. *. padding, height
  in
  let widths_heights = List.map begin fun node ->
    let width, height = draw_node 0 !x y0 node in
    x := !x +. width;
    width, height
  end nodes in
  let widths, heights = List.split widths_heights in
  let width = List.fold_left (+.) 0.0 widths in
  let height = List.fold_left max 0.0 heights in
  let points = PDFUtil.group_by (fun (x, _, _) -> x) !points in
  let points = List.map begin fun (x, g) ->
    let yw = List.fold_left (fun (ya, wa) (_, y, w) -> min ya y, max wa w) (max_float, 0.) g in
    x, yw
  end points in
  (* Draw vertical lines between column headers *)
  with_line_width doc 0.0 begin fun () ->
    let border_width = size_of_thickness border_width in
    List.iter begin fun (x', (y', lw)) ->
      if lw < max_float && x' < !x -. lw then begin (* the right-most line is on the right table border *)
        let lw = min border_width lw in (* TODO  *)
        PDF.set_line_width lw doc;
        PDF.line ~x1:x' ~y1:(y' +. lw/.2. +. line_disjoin) ~x2:x' ~y2:(y +. height -. lw/.2.) doc;
      end;
    end points;
  end ();
  height, points
;;

let has_border (border : border_part list) which =
  List.mem `All border || List.mem which border

let rec list_pos ?(pos=0) ll x = match ll with
  | [] -> raise Not_found
  | a :: b -> if a = x then pos else 1 + (list_pos ~pos b x)

let default_cell_func ~index ~row ~col = Cell_properties {
  prop_text       = (match row col with None -> "" | Some x -> x);
  prop_align      = `Center;
  prop_font_style = [];
  prop_font_size  = None;
  prop_image      = None;
  prop_bg_color   = None;
  prop_fg_color   = None;
}

let print
    ~x
    ~y
    ~width
    ~page_height
    ~columns
    ~rows
    ?header_layout
    ?(grid_lines=`Both)
    ?(border=[`All])
    ?(border_width=`Medium)
    ?(page_header_height=0.0)
    ?(page_break_func=ignore)
    ?(cellpadding=0.5)
    ?(rowspacing=0.)
    ?(line_spacing=1.0)
    ?(cell_func=default_cell_func)
    ?(use_markup=false)
    ?caption
    doc =
  let margin_top, _, _, _ = PDF.margins doc in
  let has_border = has_border border in
  let with_line_width thickness f x =
    let old = PDF.line_width doc in
    PDF.set_line_width (size_of_thickness thickness) doc;
    f x;
    PDF.set_line_width old doc
  in
  let line_disjoin = match grid_lines with `Both | `Horizontal -> 0.0 | _ -> line_disjoin in
  let vlines_points = ref [] in
  let header_height = ref 0.0 in
  let perc =
    let sum_perc = ref 0.0 in fun x ->
      if x <= 0.0 then 0.0 else
        let w = if !sum_perc +. x > 100. then 100. -. !sum_perc else width *. x /. 100. in
        sum_perc := !sum_perc +. x;
        w
  in
  List.iter (fun (_, col) -> col.col_width <- (perc col.col_width)) columns;
  let header_layout = match header_layout with Some hl -> hl
    | None -> List.map (fun (id, _) -> `Leaf id) columns
  in
  (**  *)
  let set_default_draw_color doc = PDF.set_draw_color ~red:0 ~green:0 ~blue:0 doc in
  let x0 = x in
  let y0 = y in
  let x = ref x0 in
  let y = ref y0 in
  (** column titles *)
  let print_titles ~x ~y () =
    let x0, y0 = !x, !y in
    PDF.set_font ~style:[] doc;
    let height, points = header_draw ~columns ~nodes:header_layout ~x:!x ~y:!y
      ~padding:cellpadding ~align:`Center ~line_disjoin ~border_width doc in
    x := x0;
    y := !y +. height;
    begin
      match grid_lines with `None -> () | _ ->
        (* Horizontal line between header and first row *)
        let xx = ref x0 in
        let yy = y0 +. height in
        List.iter begin fun (_, col) ->
          PDF.line ~x1:(!xx +. line_disjoin) ~y1:yy ~x2:(!xx +. col.col_width -. line_disjoin) ~y2:yy doc;
          xx := !xx +. col.col_width;
        end columns;
    end;
    PDF.set_font ~style:[] doc;
    PDF.set ~x:!x ~y:!y doc;
    height, points
  in
  (** Caption *)
  begin
    match caption with None -> () | Some caption ->
      let size = PDF.font_size doc in
      PDF.set_font ~style:[`Bold] ~size:(size *. 1.2) doc;
      PDF.set ~x:!x ~y:!y doc;
      let line_height = (PDF.font_size doc) /. PDF.scale doc in
      PDF.multi_cell ~width ~line_height ~border:[] ~align:`Center ~text:caption doc;
      PDF.set_font ~style:[] ~size doc;
      y := !y +. line_height *. 1.62;
  end;
  let top, left = ref !y, ref x0 in
  let y_avail = ref (page_height -. y0 +. margin_top +. page_header_height) in
  let hh, vp = print_titles ~x ~y () in
  vlines_points := vp;
  header_height := hh;
  (** Vertical lines between columns *)
  let print_vertical_lines ~x ~y ~bottom () =
    let xw = x +. width in
    if has_border `T then with_line_width border_width (PDF.line ~x1:x ~y1:y ~x2:xw ~y2:y) doc;
    if has_border `L then with_line_width border_width (PDF.line ~x1:x ~y1:y ~x2:x ~y2:bottom) doc;
    if has_border `B then with_line_width border_width (PDF.line ~x1:x ~y1:bottom ~x2:xw ~y2:bottom) doc;
    if has_border `R then with_line_width border_width (PDF.line ~x1:xw ~y1:y ~x2:xw ~y2:bottom) doc;
    match grid_lines with `Both | `Vertical ->
      with_line_width `Thin begin fun () ->
        (* Vertical lines between columns and below the column titles *)
        let y = y +. !header_height in
        List.iter begin fun (x', (y', lw)) ->
          if lw > 0. && lw < max_float && x' < x +. width then begin (* the right-most line is on the right border *)
            let lw = min (size_of_thickness border_width) lw in
            PDF.set_line_width lw doc;
            PDF.line ~x1:x' ~y1:(y +. lw/.2.) ~x2:x' ~y2:(bottom -. lw/.2. -. line_disjoin) doc;
          end
        end !vlines_points;
      end ()
    | _ -> ()
  in
  (** Print rows *)
  let tags = fst (List.split columns) in
  let (!!) = PDFUtil.memo ~f:(list_pos tags) in
  let index = ref 0 in
  let cont = ref [] in
  let line_height = (PDF.font_size doc) /. PDF.scale doc *. line_spacing in
  List.iter begin fun row ->
    x := !left;
    let row_content  = Array.create (Array.length row) (Cell_properties {
      prop_text       = "";
      prop_align      = `Left;
      prop_font_style = [];
      prop_font_size  = None;
      prop_image      = None;
      prop_bg_color   = None;
      prop_fg_color   = None;
    }) in
    let i = ref 0 in
    let heights = List.map begin fun (tag, col) ->
      try
        let row tag = row.(!! tag) in
        let cell = cell_func ~index:!index ~row ~col:tag in
        let maxh =
          match cell with
            | Cell_properties properties ->
              begin
                match properties.prop_image with
                  | None ->
                    let font =
                      match doc.PDFDocument.current_font with
                        | Some font -> font.PDFDocument.font_metrics
                        | _ -> failwith "No current font"
                    in
                    let text_width = PDF.get_text_width font (PDF.font_size doc) properties.prop_text in (* Works only with regular font *)
                    let line_height = match properties.prop_font_size with None -> line_height | Some x -> PDF.font_size doc /. PDF.scale doc *. line_spacing in
                    if col.col_width > 0.0 then (ceil (text_width /. col.col_width)) *. line_height else 0.0
                  | Some image ->
                    let asp = (float image.img_width) /. (float image.img_height) in
                    if col.col_width > 0.0 then col.col_width /. asp else 0.0
              end;
            | Cell_draw (height, _) -> height
        in
        row_content.(!i) <- cell;
        incr i;
        maxh
      with Not_found -> failwith "PDFTable: no such column"
    end columns in
    let row_height = List.fold_left max 0.0 heights +. 2. *. cellpadding in
    (* Salto pagina se necessario. Se una riga sconfina su più righe e l'altezza totale
     * supera lo spazio restante a fine pagina allora viene forzato un salto pagina (invece
     * di spezzare la riga). *)
    if !y -. !top +. row_height +. rowspacing > !y_avail then begin
      List.iter (fun f -> f ()) !cont;
      cont := [];
      print_vertical_lines ~x:!left ~y:!top ~bottom:!y ();
      page_break_func ();
      x := PDF.x doc;
      y := PDF.y doc;
      left := !x;
      top := !y;
      let margin_top, _, _, _ = PDF.margins doc in
      y_avail := margin_top +. page_header_height +. page_height -. PDF.y doc;
      let hh, vp = print_titles ~x ~y () in
      vlines_points := vp;
      header_height := hh;
      x := !left;
    end;
    let ys = List.map begin fun (tag, col) ->
      let width = col.col_width in
      if width > 0. then begin
        begin
          match row_content.(!! tag) with
            | Cell_properties properties ->
              let old_size = PDF.font_size doc in
              let old_style = PDF.font_style doc in
              PDF.set_font ~style:properties.prop_font_style ?size:properties.prop_font_size doc;
              PDF.set ~x:!x ~y:(!y +. cellpadding) doc;
              let line_height = match properties.prop_font_size with None -> line_height | Some x -> PDF.font_size doc /. PDF.scale doc *. line_spacing in
              begin
                match properties.prop_image with
                  | None ->
                    let x = PDF.x doc in
                    let y = PDF.y doc +. cellpadding in
                    begin
                      match properties.prop_bg_color with None -> ()
                      | Some (red, green, blue) ->
                        PDF.set_fill_color ~red ~green ~blue doc;
                        PDF.rect ~x ~y ~width ~height:line_height ~style:`Fill doc;
                    end;
                    if use_markup then begin
                      let markup = PDFMarkup.prepare ~width:(width -. 2. *. cellpadding)
                          ~line_spacing ~markup:properties.prop_text doc in
                      markup.PDFMarkup.print ~x:(x +. cellpadding) ~y ()
                    end else begin
                      PDF.multi_cell ~width:(width -. 2. *. cellpadding)
                        ~line_height ~align:properties.prop_align ~text:properties.prop_text doc;
                    end
                  | Some image ->
                    let asp = (float image.img_width) /. (float image.img_height) in
                    PDF.image
                      ~name:image.img_name
                      ~data:image.img_data
                      ~x:!x ~y:!y ~width doc;
                    PDF.set ~y:(PDF.y doc +. width /. asp) doc
              end;
              PDF.set_font ~style:old_style ~size:old_size doc;
            | Cell_draw (height, func) ->
              let width = width -. 2. *. cellpadding in
              func ~x:(!x +. cellpadding) ~y:(!y +. cellpadding) ~width ~height;
              PDF.set ~y:(!y +. height) doc
        end;
        x := !x +. width;
      end;
      PDF.y doc +. cellpadding +. rowspacing
    end columns in
    y := List.fold_left max 0.0 ys;
    (** Horizontal line between rows. *)
    begin
      match grid_lines with
        | `Horizontal | `Both ->
          let x1, y1, x2, y2 = !left, !y, !x, !y in
          let f = fun () -> PDF.line ~x1 ~y1 ~x2 ~y2 doc in
          cont := f :: !cont;
        | _ -> ()
    end;
    set_default_draw_color doc;
    incr index;
  end rows;
  List.iter (fun f -> f ()) !cont;
  print_vertical_lines ~x:!left ~y:!top ~bottom:!y ()






