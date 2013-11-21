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


open Fpdf_error
open Printf

type t = {
  doc                      : Fpdf.t;
  x0                       : float;
  y0                       : float;
  mutable x               : float;
  mutable y               : float;
  mutable rows             : int;
  mutable cols             : int;
  mutable cells            : ((int * int) * cell) list; (* row x column *)
  colwidths                : float array;
  border_width             : thickness option;
  padding                  : float;
  mutable v_lines          : (thickness option * int * int option * int) list; (* line_width, rowstart, rowstop, col *)
  mutable h_lines          : (thickness option * int * int option * int) list; (* line_width, colstart, colstop, row *)
  mutable page_breaks      : int list; (* Row indexes before whose a page break is inserted *)
  debug                    : bool;
}

and cell = {
  mutable cell_x      : float;
  mutable cell_y      : float;
  mutable width       : float;
  mutable height      : float;
  mutable cell_width  : float;
  mutable cell_height : float;
  mutable rowspan     : int;
  mutable colspan     : int;
  callback            : x:float -> y:float -> col_width:float -> row_height:float -> unit;
}

and thickness = [`Thin | `Medium | `Thick ]

let line_disjoin = 0.65

let size_of_thickness = function `Thin -> 0.01 | `Medium -> 0.25 | `Thick -> 0.5

let create ~x ~y ?(padding=0.0) ?(border_width=`Medium) ~width ~colwidths ?(debug=false) doc =
  let (!!) x = width *. x /. 100. in
  let sum = Array.fold_left (+.) 0.0 colwidths in
  if sum > 100. then
    (error Invalid_colwidths
       (sprintf "The sum of the column width percentages is greather than 100 (%F)" sum));
  let colwidths = Array.map (!!) colwidths in
  {
    doc;
    x0               = x;
    y0               = y;
    x               = x;
    y               = y;
    rows             = 0;
    cols             = 0;
    cells            = [];
    colwidths;
    border_width     = Some border_width;
    padding          = padding;
    h_lines          = [];
    v_lines          = [];
    page_breaks      = [];
    debug;
  }

let set t row col ~width ~height ?(rowspan=1) ?(colspan=1) callback =
  let cell = { cell_x=0.; cell_y=0.; width; height; rowspan; colspan; callback; cell_width=0.; cell_height=0. } in
  let coords = row, col in
  t.cells <- (coords, cell) :: (List.remove_assoc coords t.cells);
  t.rows <- max t.rows (row + 1);
  t.cols <- max t.cols (col + 1)

let set_markup =
  let open Fpdf_markup in
  let align content xalign yalign ~x ~y ~col_width ~row_height =
    let x = x +. (col_width -. content.width) *. xalign in
    let y = y +. (row_height -. content.height) *. yalign in
    content.print ~x ~y ()
  in
  fun table row col ?rowspan ?(colspan=1) ?(xalign=0.5) ?(yalign=0.5) ?padding markup ->
    let width = ref 0.0 in
    for i = col to col + colspan - 1 do
      let colwidth =
        try table.colwidths.(i)
        with Invalid_argument("index out of bounds") ->
          Fpdf_error.error No_such_column "Fpdf_tabular: number of column widths is less than number of columns in the table"
      in
      width := !width +. colwidth
    done;
    let content = Fpdf_markup.prepare ?padding ~width:!width (*~border_width:0.1 ~border_color:"#00ff00" *)~markup table.doc in
    set table row col ?rowspan ~colspan ~width:content.width ~height:content.height
      (align content xalign yalign)

let find_cell t row col =
  try Some (List.assoc (row, col) t.cells)
  with Not_found -> None

let col_width t n =
  let cells = List.filter (fun ((_, c), _) -> c = n) t.cells in
  List.fold_left (fun acc (_, cell) -> max acc cell.width) 0.0 cells

let row_height t n =
  let cells = List.filter (fun ((r, _), _) -> r = n) t.cells in
  List.fold_left (fun acc (_, cell) -> max acc cell.height) 0.0 cells

let table_width t =
  let res = ref 0.0 in
  for j = 0 to t.cols - 1 do res := !res +. col_width t j +. 2. *. t.padding done;
  !res

let table_height t =
  let res = ref 0.0 in
  for i = 0 to t.rows - 1 do res := !res +. row_height t i +. 2. *. t.padding done;
  !res

let add_vertical_line ?line_width ~rowstart ?rowstop ~col table =  table.v_lines <- (line_width, rowstart, rowstop, col) :: table.v_lines
let add_horizontal_line ?line_width ~colstart ?colstop ~row table =  table.h_lines <- (line_width, colstart, colstop, row) :: table.h_lines

let add_page_break_before row table = table.page_breaks <- row :: table.page_breaks

let line_intersection xh1 xh2 yh    xv yv1 yv2 =
  if xh1 < xv && xv < xh2 && yv1 < yh && yh < yv2 then Some xv else None

let pack t =
  (* Structure cells as matrix for not having to search them *)
  let matrix = Array.make_matrix t.rows t.cols None in
  List.iter (fun ((row, col), cell) -> matrix.(row).(col) <- Some cell) t.cells;
  (* Calculate table dimensions *)
  let col_widths = Array.create t.cols 0.0 in
  Array.iteri begin fun c _ ->
    for i = 0 to t.rows - 1 do
      col_widths.(c) <- max col_widths.(c) (match matrix.(i).(c) with Some c when c.colspan = 1 -> c.width | _ -> 0.0)
    done
  end col_widths;
  let row_heights = Array.create t.rows 0.0 in
  Array.iteri begin fun r _ ->
    for i = 0 to t.cols - 1 do
      row_heights.(r) <- max row_heights.(r) (match matrix.(r).(i) with Some c when c.rowspan = 1 -> c.height | _ -> 0.0)
    done
  end row_heights;
  (*let rowspan_heights = Array.create t.rows (0.0, 0) in
    Array.iteri begin fun r _ ->
    for i = 0 to t.cols - 1 do
      rowspan_heights.(r) <-
        match matrix.(r).(i) with
          | Some c when c.rowspan > 1 ->
            if c.height >= fst rowspan_heights.(r) then c.height, c.rowspan
            else rowspan_heights.(r)
          | _ -> rowspan_heights.(r)
    done
    end rowspan_heights;

    Array.iteri begin fun r _ ->
    ()
    end row_heights;*)
  let table_width = Array.fold_left (fun sum w -> sum +. w +. 2. *. t.padding) 0.0 col_widths in
  let table_height = Array.fold_left (fun sum h -> sum +. h +. 2. *. t.padding) 0.0 row_heights in
  let table_height_rows start stop =
    let len = stop - start + 1 in
    let hh = Array.make len 0.0 in
    Array.blit row_heights start hh 0 len;
    Array.fold_left (fun sum h -> sum +. h +. 2. *. t.padding) 0.0 hh
  in
  let thin = size_of_thickness `Thin in
  let medium = size_of_thickness `Medium in
  let v_lines = ref [] in
  let current_lw = ref 0.0 in
  let page_start_row = ref 0 in
  (* Print table border *)
  let print_table_border ~start ~stop () =
    match t.border_width with
      | Some border_width ->
        Fpdf.push_graphics_state t.doc;
        Fpdf.set_line_width (size_of_thickness border_width) t.doc;
        let height = table_height_rows start stop in
        let y = if !page_start_row = 0 then t.y0 else begin
            let margin_top, _, _, _ = Fpdf.margins t.doc in
            margin_top
          end in
        Fpdf.rect ~x:t.x0 ~y ~width:table_width ~height(*:table_height*) t.doc;
        Fpdf.pop_graphics_state t.doc;
      | _ -> ()
  in
  (* Draw grid vertical lines *)
  let print_vertical_lines ~pstart ~pstop () =
    Fpdf.push_graphics_state t.doc;
    List.iter begin fun (lw, rowstart, rowstop, col) ->
      try
        let rowstart =
          if pstart <= rowstart && rowstart <= pstop then rowstart
          else if rowstart <= pstart then pstart
          else if rowstart > pstop then raise Exit else raise Exit
        in
        begin
          match matrix.(rowstart).(col) with
            | Some c1 ->
              begin
                let rowstop = match rowstop with Some x -> x | _ -> pstop in
                let rowstop =
                  if rowstop < pstart then raise Exit
                  else if rowstop <= pstop then rowstop
                  else pstop
                in
                let x = c1.cell_x -. t.padding in
                let y1 = c1.cell_y -. t.padding +. line_disjoin in
                let x, y1, y2 =
                  match matrix.(rowstop).(col) with
                    | Some c2 ->
                      let y2 = c2.cell_y +. c2.cell_height +. t.padding -. line_disjoin in
                      x, y1, y2;
                    | _ ->
                      let y_start =
                        if !page_start_row = 0 then t.y0 else begin
                          let margin_top, _, _, _ = Fpdf.margins t.doc in
                          margin_top
                        end
                      in
                      let y2 = y_start +. (table_height_rows pstart pstop) -. line_disjoin in
                      x, y1, y2
                in
                let lw = match lw with Some x -> size_of_thickness x | _ -> if rowstart = 0 then medium else thin in
                if lw <> !current_lw then begin
                  current_lw := lw;
                  Fpdf.set_line_width lw t.doc;
                end;
                v_lines := ((pstart, pstop), x, y1, x, y2) :: !v_lines;
                Fpdf.line ~x1:x ~y1 ~x2:x ~y2 t.doc;
              end;
            | _ -> ()
        end;
      with Exit -> ()
    end t.v_lines;
    Fpdf.pop_graphics_state t.doc;
  in
  (* Draw grid horizontal lines *)
  let print_horizontal_lines ~pstart ~pstop () =
    List.iter begin fun (lw, colstart, colstop, row) ->
      if pstart <= row && row <= pstop then
        match matrix.(row).(colstart) with
          | Some c1 ->
            Fpdf.push_graphics_state t.doc;
            let colstop = match colstop with Some x -> x | _ -> t.cols - 1 in
            let stop =
              match matrix.(row).(colstop) with
                | Some c2 -> c2.cell_x +. c2.cell_width +. t.padding
                | _ -> t.x0 +. table_width
            in
            let y = c1.cell_y -. t.padding in
            let x1 = c1.cell_x -. t.padding  in
            let x2 = stop in
            let lw = match lw with Some x -> size_of_thickness x | _ -> if colstart = 0 then medium else thin in
            let i_points =
              List.rev (List.fold_left begin fun acc ((p1, p2), xv, yv1, _, yv2) ->
                  if p1 = pstart && p2 = pstop then begin
                    (*Printf.printf "===> %F -- %F (%F)\n%!" yv1 yv2 xv;*)
                    match line_intersection x1 x2 y xv yv1 yv2 with
                      | Some x -> x :: acc
                      | _ -> acc
                  end else acc
                end [] (List.sort (fun (_, x1, _, _, _) (_, x2, _, _, _) -> compare x1 x2) !v_lines));
            in
            let segments = List.combine (x1 :: i_points) (List.concat [i_points; [x2]]) in
            if lw <> !current_lw then begin
              current_lw := lw;
              Fpdf.set_line_width lw t.doc;
            end;
            List.iter begin fun (x1, x2) ->
              let x1 = x1 +. line_disjoin in
              let x2 = x2 -. line_disjoin in
              Fpdf.line ~x1 ~y1:y ~x2 ~y2:y t.doc;
              (*Printf.printf "---------->H %d, %d -- %F -- %F (%F)\n%!" pstart pstop x1 x2 y;*)
            end segments;
            Fpdf.pop_graphics_state t.doc;
          | _ -> ();
    end t.h_lines;
  in
  (* Print cell contents *)
  t.y <- t.y0;
  Array.iteri begin fun i row ->
    (* Page Break *)
    if List.mem i t.page_breaks then begin
      print_table_border ~start:!page_start_row ~stop:(i - 1) ();
      print_vertical_lines ~pstart:!page_start_row ~pstop:(i - 1) ();
      print_horizontal_lines ~pstart:!page_start_row ~pstop:(i - 1) ();
      v_lines := [];
      Fpdf.add_page t.doc;
      let margin_top, _, _, _ = Fpdf.margins t.doc in
      page_start_row := i;
      t.y <- margin_top
    end;
    (* Print row *)
    t.x <- t.x0;
    let rh = row_heights.(i) in
    t.y <- t.y +. t.padding;
    Array.iteri begin fun j cell ->
      let cw = col_widths.(j) in
      t.x <- t.x +. t.padding;
      begin
        match cell with
          | Some cell ->
            let col_width = if cell.colspan = 1 then cw else cell.width +. t.padding *. 2. *. (float cell.colspan -. 1.) in
            let row_height =
              if cell.rowspan = 1 then rh
              else begin
                let h = ref 0.0 in
                for k = i to i + cell.rowspan - 1 do h := !h +. row_heights.(k) done;
                !h +. 2. *. t.padding *. (float cell.rowspan -. 1.);
              end
            in
            cell.callback ~x:t.x ~y:t.y ~col_width ~row_height;
            cell.cell_width <- col_width;
            cell.cell_height <- row_height;
            cell.cell_x <- t.x;
            cell.cell_y <- t.y;
            if t.debug then begin
              Fpdf_graphics_state.push t.doc;
              Fpdf.set_text_color ~red:150 ~green:150 ~blue:150 t.doc;
              Fpdf.set_fill_color ~red:150 ~green:150 ~blue:150 t.doc;
              Fpdf.set_draw_color ~red:250 ~green:150 ~blue:150 t.doc;
              let radius = 3.0 *. line_disjoin in
              Fpdf.rect ~x:t.x ~y:t.y ~width:cell.cell_width ~height:cell.cell_height ~radius:(radius, radius, radius, radius) t.doc;
              Fpdf.line ~x1:t.x ~y1:t.y ~x2:(t.x +. cell.cell_width) ~y2:(t.y +. cell.cell_height) t.doc;
              Fpdf.line ~x1:(t.x +. cell.cell_width) ~y1:t.y ~x2:t.x ~y2:(t.y +. cell.cell_height) t.doc;
              Fpdf.set ~x:cell.cell_x ~y:(cell.cell_y +. 1.) t.doc;
              Fpdf.cell ~width:10. ~font_size:3. ~font_family:`Helvetica ~font_style:[`Italic]
                ~text:(sprintf "(%.1d, %.1d) h=%.1f y=%.1f"
                         i j row_heights.(i) cell.cell_y) t.doc;
              Fpdf_graphics_state.pop t.doc;
            end
          | _ -> ()
      end;
      t.x <- t.x +. cw +. t.padding;
    end row;
    (* Increment Y *)
    t.y <- t.y +. rh +. t.padding;
  end matrix;
  print_table_border ~start:!page_start_row ~stop:(t.rows - 1) ();
  print_vertical_lines ~pstart:!page_start_row ~pstop:(t.rows - 1) ();
  print_horizontal_lines ~pstart:!page_start_row ~pstop:(t.rows - 1) ();
;;









