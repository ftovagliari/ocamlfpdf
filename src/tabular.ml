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


open PDFError
open Printf

type t = {
  doc             : PDF.t;
  x0              : float;
  y0              : float;
  mutable rows    : int;
  mutable cols    : int;
  mutable cells   : ((int * int) * cell) list; (* row x column *)
  colwidths       : float array;
  border_width    : thickness option;
  padding         : float;
  mutable v_lines : (thickness option * int * int option * int) list; (* line_width, rowstart, rowstop, col *)
  mutable h_lines : (thickness option * int * int option * int) list; (* line_width, colstart, colstop, row *)
  debug           : bool;
}

and cell = {
  mutable x           : float;
  mutable y           : float;
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

let size_of_thickness = function `Thin -> 0.1 | `Medium -> 0.25 | `Thick -> 0.5

let create ~x ~y ?(padding=0.0) ?(border_width=`Medium) ~width ~colwidths ?(debug=false) doc =
  let (!!) x = width *. x /. 100. in
  let sum = Array.fold_left (+.) 0.0 colwidths in
  if sum > 100. then
    (error Invalid_colwidths
       (sprintf "The sum of the column width percentages is greather than 100 (%F)" sum));
  let colwidths = Array.map (!!) colwidths in
  {
    doc;
    x0           = x;
    y0           = y;
    rows         = 0;
    cols         = 0;
    cells        = [];
    colwidths;
    border_width = Some border_width;
    padding      = padding;
    h_lines      = [];
    v_lines      = [];
    debug;
  }

let set t row col ~width ~height ?(rowspan=1) ?(colspan=1) callback =
  let cell = { x=0.; y=0.; width; height; rowspan; colspan; callback; cell_width=0.; cell_height=0. } in
  let coords = row, col in
  t.cells <- (coords, cell) :: (List.remove_assoc coords t.cells);
  t.rows <- max t.rows (row + 1);
  t.cols <- max t.cols (col + 1)

let set_markup =
  let open PDFMarkup in
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
          PDFError.error No_such_column "Tabular: number of column widths is less than number of columns in the table"
      in
      width := !width +. colwidth
    done;
    let content = PDFMarkup.prepare ?padding ~width:!width (*~border_width:0.1 ~border_color:"#00ff00" *)~markup table.doc in
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
  (* Print table border *)
  begin
    match t.border_width with
      | Some border_width ->
        PDF.push_graphics_state t.doc;
        PDF.set_line_width (size_of_thickness border_width) t.doc;
        PDF.rect ~x:t.x0 ~y:t.y0 ~width:table_width ~height:table_height t.doc;
        PDF.pop_graphics_state t.doc;
      | _ -> ()
  end;
  (* Print cell contents *)
  let y = ref t.y0 in
  Array.iteri begin fun i row ->
    let x = ref t.x0 in
    let rh = row_heights.(i) in
    y := !y +. t.padding;
    Array.iteri begin fun j cell ->
      let cw = col_widths.(j) in
      x := !x +. t.padding;
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
            cell.callback ~x:!x ~y:!y ~col_width ~row_height;
            cell.cell_width <- col_width;
            cell.cell_height <- row_height;
            cell.x <- !x;
            cell.y <- !y;
            if t.debug then begin
              PDFGraphicsState.push t.doc;
              PDF.set_text_color ~red:150 ~green:150 ~blue:150 t.doc;
              PDF.set_fill_color ~red:150 ~green:150 ~blue:150 t.doc;
              PDF.set_draw_color ~red:150 ~green:150 ~blue:150 t.doc;
              PDF.rect ~x:!x ~y:!y ~width:cell.cell_width ~height:cell.cell_height ~radius:(3.0 *. line_disjoin) t.doc;
              PDF.line ~x1:!x ~y1:!y ~x2:(!x +. cell.cell_width) ~y2:(!y +. cell.cell_height) t.doc;
              PDF.line ~x1:(!x +. cell.cell_width) ~y1:!y ~x2:!x ~y2:(!y +. cell.cell_height) t.doc;
              PDF.set ~x:cell.x ~y:(cell.y +. 1.) t.doc;
              PDF.cell ~width:10. ~font_size:4. ~font_style:[`Italic] ~text:(sprintf "(%d, %d) h=%F" i j row_heights.(i)) t.doc;
              PDFGraphicsState.pop t.doc;
            end
          | _ -> ()
      end;
      x := !x +. cw +. t.padding;
    end row;
    y := !y +. rh +. t.padding;
  end matrix;
  (* Draw grid vertical lines *)
  let thin = size_of_thickness `Thin in
  let medium = size_of_thickness `Medium in
  let v_lines = ref [] in
  PDF.push_graphics_state t.doc;
  let current_lw = ref 0.0 in
  List.iter begin fun (lw, rowstart, rowstop, col) ->
    match matrix.(rowstart).(col) with
      | Some c1 ->
        begin
          let rowstop = match rowstop with Some x -> x | _ -> t.rows - 1 in
          let x = c1.x -. t.padding in
          let y1 = c1.y -. t.padding +. line_disjoin in
          let x, y1, y2 =
            match matrix.(rowstop).(col) with
              | Some c2 ->
                let y2 = c2.y +. c2.cell_height +. t.padding -. line_disjoin in
                x, y1, y2;
              | _ ->
                let y2 = t.y0 +. table_height -. line_disjoin in
                x, y1, y2
          in
          let lw = match lw with Some x -> size_of_thickness x | _ -> if rowstart = 0 then medium else thin in
          if lw <> !current_lw then begin
            current_lw := lw;
            PDF.set_line_width lw t.doc;
          end;
          v_lines := (x, y1, x, y2) :: !v_lines;
          PDF.line ~x1:x ~y1 ~x2:x ~y2 t.doc;
        end;
      | _ -> ()
  end t.v_lines;
  (* Draw grid horizontal lines *)
  List.iter begin fun (lw, colstart, colstop, row) ->
    match matrix.(row).(colstart) with
      | Some c1 ->
        let colstop = match colstop with Some x -> x | _ -> t.cols - 1 in
        let stop =
          match matrix.(row).(colstop) with
            | Some c2 -> c2.x +. c2.cell_width +. t.padding
            | _ -> t.x0 +. table_width
        in
        let y = c1.y -. t.padding in
        let x1 = c1.x -. t.padding  in
        let x2 = stop in
        let lw = match lw with Some x -> size_of_thickness x | _ -> if colstart = 0 then medium else thin in
        let i_points =
          List.rev (List.fold_left begin fun acc (xv, yv1, _, yv2) ->
              match line_intersection x1 x2 y xv yv1 yv2 with
                | Some x' -> x' :: acc
                | _ -> acc
            end [] !v_lines);
        in
        let segments = List.combine (x1 :: i_points) (List.concat [i_points; [x2]]) in
        if lw <> !current_lw then begin
          current_lw := lw;
          PDF.set_line_width lw t.doc;
        end;
        List.iter begin fun (x1, x2) ->
          let x1 = x1 +. line_disjoin in
          let x2 = x2 -. line_disjoin in
          PDF.line ~x1 ~y1:y ~x2 ~y2:y t.doc;
        end segments
      | _ -> ();
  end t.h_lines;
  PDF.pop_graphics_state t.doc;
;;









