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

open PDFTypes
open PDFDocument
open PDFPage
open PDFUtil
open Image
open Printf

(** get_string_width *)
let get_string_width s doc =
  let font = match doc.current_font with
    | None -> failwith "get_string_width: no current font."
    | Some f -> f in
  let cw = font.font_cw in
  let width = ref 0 in
  String.iter begin fun c ->
    width := !width + (cw c)
  end s;
  (float !width) *. doc.font_size /. 1000.;;

(** TODO *)
let do_underline x y txt = ""

(** Color *)
let set_text_color ~red ?(green=(-1)) ?(blue=(-1)) doc =
  if (red = 0 && green = 0 && blue = 0) || green = -1 then
    (doc.textColor <- sprintf "%.3f g" ((float red) /. 255.))
  else begin doc.textColor <- sprintf "%.3f %.3f %.3f rg"
    ((float red) /. 255.) ((float green) /. 255.) ((float blue) /. 255.)
  end;
  doc.colorFlag <- doc.fillColor <> doc.textColor;
  doc.text_color_rgb <- (red, green, blue)

let text_color doc = doc.text_color_rgb

(** text *)
let text ~x ~y ~text doc =
  let s = ref (sprintf "BT %.2f %.2f Td (%s) Tj ET"
    (x *. doc.k) ((doc.h -. y) *. doc.k) (escape text)) in
  if doc.underline  && text <> "" then s := !s ^ " " ^ (do_underline x y text);
  if doc.colorFlag then s:= "q " ^ doc.textColor ^ " " ^ !s ^ " Q";
  print_buffer doc "%s\n" !s;;

let newline ?height doc =
  doc.pos_x <- doc.l_margin;
  match height with
    | None -> doc.pos_y <- doc.pos_y +. doc.lasth
    | Some h -> doc.pos_y <- doc.pos_y +. h

let cell
    ~width
    ?(height=0.)
    ?(text="")
    ?(border : border_part list option)
    ?padding
    ?(ln=(`Right : [`Right | `Next_line | `Bottom]))
    ?(align=`Left)
    ?(fill=false)
    ?(link="")
    doc =
  let padding = match padding with None -> doc.c_margin | Some padding -> padding in
  let scale = doc.k in
  if doc.pos_y +. height > doc.pageBreakTrigger && (not doc.inFooter) && (doc.auto_page_break) then begin
    let x0 = doc.pos_x in
    let word_spacing = doc.ws in
    if word_spacing > 0. then begin
      doc.ws <- 0.;
      print_buffer doc "0 Tw\n"
    end;
    add_page ~orientation:doc.cur_orientation doc;
    doc.pos_x <- x0;
    if word_spacing > 0. then begin
      doc.ws <- word_spacing;
      print_buffer doc "%.3f Tw\n" (word_spacing *. scale)
    end;
  end;
  let text_width = get_string_width text in
  let width = if width = 0. then doc.w -. doc.r_margin -. doc.pos_x else width in
  (* Frame *)
  let border, frame = match border with
    | None -> [], false
    | Some b -> if List.mem `All b then [`All], true else b, false in
  if fill || frame then begin
    let op = if fill && frame then "B"
      else if fill && not frame then "f"
      else "S" in (* frame && not fill *)
    print_buffer doc "%.2f %.2f %.2f %.2f re %s "
      (doc.pos_x *. scale) ((doc.h -. doc.pos_y) *. scale) (width *. scale) (-.(height) *. scale) op
  end;
  (* The code string *)
  let code = ref "" in
  (* Borders *)
  if not frame then begin
    let border = List.sort compare (remove_dupl border) in
    let x0, y0 = doc.pos_x, doc.pos_y in
    let sprintf = sprintf "%.2f %.2f m %.2f %.2f l S " in
    let border_code = List.map begin function
      | `L -> sprintf (x0 *. scale) ((doc.h -. y0) *. scale) (x0 *. scale) ((doc.h -. (y0 +. height)) *. scale)
      | `T -> sprintf (x0 *. scale) ((doc.h -. y0) *. scale) ((x0 +. width) *. scale) ((doc.h -. y0) *. scale)
      | `R -> sprintf ((x0 +. width) *. scale) ((doc.h -. y0) *. scale) ((x0 +. width) *. scale)
        ((doc.h -. (y0 +. height)) *. scale)
      | `B -> sprintf (x0 *. scale) ((doc.h -. (y0 +. height)) *. scale)
        ((x0 +. width) *. scale) ((doc.h -. (y0 +. height)) *. scale)
      | _ -> ""
    end border in
    code := String.concat "" border_code
  end;
  (* Text *)
  let posy = doc.pos_y +. 0.5 *. height +. 0.3 *. doc.font_size in
  (*let posy = doc.pos_y +. 0.75 *. height in*)
  if (String.length text) > 0 then begin
    let dx = match align with
      | `Left | `Justified -> padding
      | `Center -> (width -. text_width doc) /. 2.
      | `Right -> width -. padding -. text_width doc in
    if doc.colorFlag then code := !code ^ "q " ^ doc.textColor ^ " ";
    code := !code ^ (sprintf "BT %.2f %.2f Td (%s) Tj ET" ((doc.pos_x +. dx) *. scale)
      ((doc.h -. posy) *. scale) (escape text));
    if doc.underline then code := !code ^
      (" " ^ (do_underline (doc.pos_x +. dx) posy text));
    if doc.colorFlag then code := !code ^ " Q";
    if (String.length link) > 0 then
      add_link (doc.pos_x +. dx) posy text_width doc.font_size link ();
  end;
  if (String.length !code) > 0 then print_buffer doc "%s\n" !code;
  doc.lasth <- height;
  (* ln *)
  match ln with
    | `Right -> doc.pos_x <- doc.pos_x +. width
    | `Bottom -> doc.pos_y <- doc.pos_y +. height
    | `Next_line ->
      doc.pos_y <- doc.pos_y +. height;
      doc.pos_x <- doc.l_margin;;

let write ~height ?padding ~text ?link doc =
  let padding = match padding with None -> doc.c_margin | Some padding -> padding in
  let cw = match doc.current_font with None -> assert false | Some font -> font.font_cw in
  let width = ref (doc.w -. doc.r_margin -. doc.pos_x) in
  let wmax = ref ((!width -. 2. *. padding) *. 1000. /. doc.font_size) in
  let text = Str.global_replace re_cr "" text in
  let nb = String.length text in
  let sep = ref (-1) and i = ref 0 and j = ref 0 and l = ref 0.0 and nl = ref 1 in
  while !i < nb do
    try
      let c = text.[!i] in
      if c = '\n' then begin (* Explicit line break *)
        cell ~width:!width ~height ~text:(String.sub text !j (!i - !j)) ~ln:`Bottom ?link doc;
        incr i;
        sep := -1;
        j := !i;
        l := 0.0;
        if !nl = 1 then begin
          doc.pos_x <- doc.l_margin;
          width := doc.w -. doc.r_margin -. doc.pos_x;
          wmax := (!width -. 2. *. padding) *. 1000. /. doc.font_size;
        end;
        incr nl;
        raise Continue;
      end;
      if c = ' ' then (sep := !i);
      l := !l +. float (cw c);
      if !l > !wmax then begin (* Automatic line break *)
        if !sep = (-1) then begin
          if doc.pos_x > doc.l_margin then begin (* Move to next line *)
            doc.pos_x <- doc.l_margin;
            doc.pos_y <- doc.pos_y +. height;
            width := doc.w -. doc.r_margin -. doc.pos_x;
            wmax := (!width -. 2. *. padding) *. 1000. /. doc.font_size;
            incr i;
            incr nl;
            raise Continue;
          end;
          if !i = !j then (incr i);
          cell ~width:!width ~height ~text:(String.sub text !j (!i - !j)) ~ln:`Bottom ?link doc;
        end else begin
          cell ~width:!width ~height ~text:(String.sub text !j (!sep - !j)) ~ln:`Bottom ?link doc;
          i := !sep + 1;
        end;
        sep := -1;
        j := !i;
        l := 0.0;
        if !nl = 1 then begin
          doc.pos_x <- doc.l_margin;
          width := doc.w -. doc.r_margin -. doc.pos_x;
          wmax := (!width -. 2. *. padding) *. 1000. /. doc.font_size;
        end;
        incr nl;
      end else (incr i);
    with Continue -> ()
  done;
  (* Last chunk *)
  if !i <> !j then begin
    cell ~width:(!l /. 1000. *. doc.font_size) ~height ~text:(String.sub text !j (String.length text - !j)) ~ln:`Right ?link doc;
  end;;

let multi_cell' ~width ~line_height ~text ?border ?padding ?(align=(`Left : align)) ?(fill = false) ?(printing=true) doc =
  let height = line_height in
  let padding = match padding with None -> doc.c_margin | Some x -> x in
  let cw = match doc.current_font with None -> failwith "multi_cell: no current font."
    | Some f -> f.font_cw in
  let text_lines = ref [] in
  let width = if width = 0. then doc.w -. doc.r_margin -. doc.pos_x else width in
  let wmax = (width -. 2. *. padding) *. 1000. /. doc.font_size in
  let text = Str.global_replace re_cr "" text in
  let nb = String.length text in
  let nb = if nb > 0 && text.[nb - 1] = '\n' then nb - 1 else nb in
  let old_ws = ref doc.ws in
  let old_pos_x = ref doc.pos_x in
  let old_pos_y = ref doc.pos_y in
  let old_lasth = ref doc.lasth in
  let border, b, b2 = match border with
    | None -> [], ref [], []
    | Some border ->
      let border = List.sort compare (remove_dupl border) in
      if List.mem `All border then [`L; `T; `R; `B], ref [`L; `R; `T;], [`L; `R]
      else
        let b2 = List.filter (function `L | `R -> true | _ -> false) border in
        let b = if List.mem `T border then `T :: b2 else b2 in
        border, ref b, b2 in
  let sep, i, j, l, ns, nl = ref (-1), ref 0, ref 0, ref 0, ref 0, ref 1 in
  let ls = ref 0 in
  while !i < nb do
    let c = text.[!i] in
    if c = '\n' then begin
      if doc.ws > 0. then begin
        doc.ws <- 0.;
        if printing then (print_buffer doc "0 Tw\n");
      end;
      let text = String.sub text !j (!i - !j) in
      if printing then (cell ~width ~height ~text ~border:(!b) ~padding ~ln:`Bottom ~align ~fill doc);
      text_lines := text :: !text_lines;
      incr i;
      sep := -1;
      j := !i;
      l := 0;
      ns := 0;
      incr nl;
      if (border <> []) && !nl = 2 then b := b2;
      (*raise Continue*)
    end else if c = ' ' then begin
      sep := !i;
      ls := !l;
      incr ns
    end;
    l := !l + (cw c);
    if (float !l) > wmax then begin
      (* Automatic line break *)
      if !sep = -1 then begin
        if !i = !j then incr i;
        if doc.ws > 0. then begin
          doc.ws <- 0.;
          if printing then (print_buffer doc "0 Tw\n");
        end;
        let text_line = String.sub text !j (!i - !j) in
        if printing then (cell ~width ~height ~text:text_line ~border:(!b) ~padding ~ln:`Bottom ~align ~fill doc);
        text_lines := text_line :: !text_lines;
      end else begin
        if align = `Justified then begin
          doc.ws <- if !ns > 1 then
            (wmax -. (float !ls)) /. 1000. *. doc.font_size /. (float (!ns - 1)) else 0.;
          if printing then (print_buffer doc "%.3f Tw\n" (doc.ws *. doc.k))
        end;
        let text_line = String.sub text !j (!sep - !j) in
        if printing then (cell ~width ~height ~text:text_line ~border:(!b) ~padding ~ln:`Bottom ~align ~fill doc);
        text_lines := text_line :: !text_lines;
        i := !sep + 1;
      end;
      sep := -1;
      j := !i;
      l := 0;
      ns := 0;
      incr nl;
      if (border <> []) && !nl = 2 then b := b2;
    end else incr i
  done;
  (* Last chunk *)
  if doc.ws > 0. then begin
    doc.ws <- 0.;
    if printing then (print_buffer doc "0 Tw\n")
  end;
  if (border <> []) && (List.mem `B border) then b := `B :: !b;
  let text_line = String.sub text !j (!i - !j) in
  if printing then (cell ~width ~height ~text:text_line ~border:(!b) ~padding ~ln:`Bottom ~align ~fill doc);
  text_lines := text_line :: !text_lines;
  if printing then (doc.pos_x <- doc.l_margin) else begin
    doc.ws <- !old_ws;
    doc.pos_x <- !old_pos_x;
    doc.pos_y <- !old_pos_y;
    doc.lasth <- !old_lasth;
  end;
  List.rev !text_lines;;

let multi_cell ~width ~line_height ~text ?border ?padding ?align ?fill doc =
  ignore (multi_cell' ~width ~line_height ~text ?border ?padding ?align ?fill ~printing:true doc)

let multi_cell_lines ~width ~line_height ~text ?border ?padding ?align ?fill doc =
  multi_cell' ~width ~line_height ~text ?border ?padding ?align ?fill ~printing:false doc

