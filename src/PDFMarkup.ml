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

type analysis = {
  mutable width                          : float;
  mutable height                         : float;
  mutable paragraphs                     : paragraph list;
}

and cell = {
  mutable text                           : string;
  mutable attr                           : attributes;
  mutable cell_width                     : float;
  mutable cell_height                    : float;
  mutable par                            : int;
  mutable line                           : int;
}

and line = {
  mutable line_width                     : float;
  mutable line_height                    : float;
  mutable line_cells                     : cell list;
}

and paragraph = {
  mutable paragraph_align                : float;
  mutable paragraph_line_spacing         : float;
  paragraph_lines                        : line list;
}

and attributes = {
  mutable family                         : Font.family;
  mutable style                          : Font.style list;
  mutable size                           : float;
  mutable scale                          : float option;
  mutable underline                      : [`NONE | `SINGLE | `LOW ];
  mutable color                          : string;
  mutable bgcolor                        : string option;
  mutable align                          : float;
  mutable line_spacing                   : float;
}

exception Width_is_less_than_first_char_width

(** Util *)

let (!!) x = match x with Some x -> x | _ -> ""

let group_by f ll =
  List.fold_right begin fun cell acc ->
    match acc with
      | ((c :: _) as p) :: tl ->
        if f c cell then ((cell :: p) :: tl) else ([cell] :: p :: tl)
      | [] -> [cell] :: []
      | _ -> assert false
  end ll [];;

let underline_of_string name =
  let name = String.lowercase name in
  match name with
    | "none" -> `NONE
    | "single" -> `SINGLE
    | "low" -> `LOW
    | _ -> invalid_arg "PDFMarkup.underline_of_string";;

let split_attrib = let re = Str.regexp "[,;][ ]*" in Str.split re;;

let char_width ~family ~style ~size ~scale ?(text_scale=1.) =
  let key = Font.key_of_font style family  in
  let f = Font.get_metric key in
  function '\n' -> 0.0 | x ->
    (((float (f x)) /. 1000.) *. size /. scale) *. text_scale ;;

let find_word_bound_backward, find_word_bound_forward =
  let blanks = Str.regexp "[- \r\n\t]" in
  let non_blanks = Str.regexp "[^ ]" in
  begin fun (bound : [`START | `STOP]) text i ->
    let i = if i < 0 then 0 else i in
    match bound with
      | `START ->
        let stop = try Str.search_backward non_blanks text i with Not_found -> i in
        (try (Str.search_backward blanks text stop) + 1 with Not_found -> 0)
      | `STOP ->
        let start = try Str.search_backward blanks text i with Not_found -> i in
        (try (Str.search_backward non_blanks text start) + 1 with Not_found -> 0)
  end, begin fun (bound : [`START | `STOP]) text i ->
    let i = if i < 0 then 0 else i in
    match bound with
      | `START ->
        (try Str.search_forward non_blanks text i with Not_found -> i)
      | `STOP -> assert false
  end;;

let default_family doc = match PDF.font_family doc with Some x -> x | _ -> `Courier;;

(** markup_to_blocks *)
let markup_to_blocks ~markup doc =
  let attr doc = {
    family    = (default_family doc);
    style     = [];
    size      = PDF.font_size doc;
    scale     = None;
    underline = `NONE;
    color     = (PDFUtil.hex_of_rgb (PDF.text_color doc));
    bgcolor   = None;
    align     = 0.0;
    line_spacing = 0.0;
  } in
  let cell ~text ~attr ~par = {text=text; attr=attr; cell_width=0.0; cell_height=0.0; par=par; line=0} in
  let markup = Str.global_replace (Str.regexp "\n") "<BR/>" markup in
  let xml = Xml.parse_string ("<MARKUP>" ^ markup ^ "</MARKUP>") in
  (*(Printf.printf "%s\n----------------------------------------\n%!" (Xml.to_string_fmt xml));*)
  let cells = ref [] in
  let current_par = ref 0 in
  let newline () =
    incr current_par;
    match !cells with
      | hd :: _ when String.length hd.text > 0 && hd.text.[String.length hd.text - 1] <> '\n' ->
        hd.text <- hd.text ^ "\n"
      | _ ->
        cells := (cell ~text:"\n" ~attr:(attr doc) ~par:(!current_par - 1)) :: !cells
  in
  Xml.iter begin function
    | Xml.PCData "&nbsp;" ->
      cells := (cell ~text:" " ~attr:(attr doc) ~par:!current_par) :: !cells;
    | Xml.PCData text ->
      cells := (cell ~text ~attr:(attr doc) ~par:!current_par) :: !cells;
    | Xml.Element (tag, _, _) when (String.uppercase tag) = "BR" ->
      newline ()
    | Xml.Element (tag, attrs, children) when (String.uppercase tag) = "SPAN" ->
      let attr = {
        family    = (try Font.family_of_string (List.assoc "family" attrs) with Not_found -> default_family doc);
        style     = (try List.map Font.style_of_string (split_attrib (List.assoc "style" attrs)) with Not_found -> []);
        size      = (try float_of_string (List.assoc "size" attrs) with Not_found -> PDF.font_size doc);
        scale     = (try Some (float_of_string (List.assoc "scale" attrs) /. 100.) with Not_found -> None);
        underline = (try underline_of_string (List.assoc "underline" attrs) with Not_found -> `NONE);
        color     = (try List.assoc "color" attrs with Not_found -> PDFUtil.hex_of_rgb (PDF.text_color doc));
        bgcolor   = (try Some (List.assoc "bgcolor" attrs) with Not_found -> None);
        align     = (try float_of_string (List.assoc "align" attrs) with Not_found -> 0.0);
        line_spacing = (try float_of_string (List.assoc "line_spacing" attrs) with Not_found -> 0.0);
      } in
      begin
        match children with
          | [] ->
            let cell = (cell ~text:" " ~attr ~par:!current_par) in
            cells := cell :: !cells;
          | children ->
            List.iter begin function
              | Xml.PCData text ->
                let cell = (cell ~text ~attr ~par:!current_par) in
                cells := cell :: !cells;
              | Xml.Element (tag, _, _) when (String.uppercase tag) = "BR" ->
                newline ()
              | _ -> invalid_arg "markup"
            end children
      end;
    | Xml.Element (tag, _, []) -> ()
    | _ -> invalid_arg "markup"
  end xml;
  List.rev !cells
;;

(** split_text_by_width *)
let split_text_by_width ~widths ~cw ~can_wrap_char text =
  let current_width = ref 0.0 in
  let chunks = ref [] in
  let i0 = ref 0 in
  let i = ref 0 in
  let widths = ref widths in
  let avail_width = ref 0.0 in
  let string_width s =
    let w = ref 0.0 in
    String.iter (fun x -> w := !w +. cw x) s;
    !w
  in
  let pop () =
    match !widths with
      | [] -> ()
      | w :: tl ->
        avail_width := w;
        widths := tl
  in
  pop();
  let len = String.length text in
  while !i < len do
    let ch = String.unsafe_get text !i in
    let w = cw ch in
    current_width := !current_width +. w;
    if !current_width > !avail_width then begin
      if !i = 0 then (raise Width_is_less_than_first_char_width);
      let is_blank = ch = ' ' in
      let word_start, need_wrap_char =
        if is_blank then (find_word_bound_forward `START text !i, false)
        else begin
          let word_start = find_word_bound_backward `START text !i in
          let s = String.sub text word_start (!i - word_start + 1) in
          let need_wrap_char = (*word_start = 0*) string_width s > !avail_width in
          word_start, need_wrap_char
        end
      in
      let remaining_width =
        if need_wrap_char && can_wrap_char then begin
          chunks := (!current_width -. w, (String.sub text !i0 (!i - !i0))) :: !chunks;
          w
        end else begin
          let word_stop = if need_wrap_char then word_start else find_word_bound_backward `STOP text !i in
          let exceeding = String.sub text word_stop (!i - word_stop + 1) in
          let exceeding_width = string_width exceeding in
          begin
            (*try*)
              chunks := (!current_width -. exceeding_width, (String.sub text !i0 (word_stop - !i0))) :: !chunks;
            (*with Invalid_argument("String.sub") ->
              Printf.printf "need_wrap_char=%b; can_wrap_char=%b; i0=%d; i=%d; word_start=%d; word_stop=%d; %S;\n%!"
                need_wrap_char !can_wrap_char !i0 !i word_start word_stop text;
              List.iter (fun (x, y) -> Printf.printf "%s\n%!" y;) !chunks;*)
          end;
          i := word_start - 1; (* -1 because i will be incremented *)
          0.0
        end
      in
      i0 := !i + 1;
      current_width := remaining_width;
      pop();
    end;
    incr i;
  done;
  chunks := (!current_width, (String.sub text !i0 (!i - !i0))) :: !chunks;
  (*List.rev*) !chunks
;;

(** split_blocks_at_line_break *)
let split_blocks_at_line_break ~paragraphs ~avail_width doc =
  let scale = PDF.scale doc in
  let width0 = avail_width in
  let first_block = ref true in
  List.map begin fun paragraph ->
    let avail_width = ref width0 in
    first_block := true;
    List.rev (List.fold_left begin fun acc block ->
      block.cell_height <- block.attr.size /. scale +. block.attr.line_spacing;
      let family = block.attr.family in
      let text_scale = block.attr.scale in
      let cw = char_width ~family ~style:block.attr.style ~size:block.attr.size ~scale ?text_scale in
      (*Printf.printf ">>> %5.2f %5.2f %S\n%!" block.attr.size !avail_width block.text;*)
      let chunks =
        try split_text_by_width ~widths:[!avail_width; width0] ~cw ~can_wrap_char:!first_block block.text
        with Width_is_less_than_first_char_width -> begin
          avail_width := width0;
          split_text_by_width ~widths:[!avail_width] ~cw ~can_wrap_char:!first_block block.text
        end
      in
      List.iter begin fun (w, t) ->
        avail_width := !avail_width -. w;
        if !avail_width < 0.0 then (avail_width := width0 -. w);
      end (List.rev chunks);
      first_block := false;
      (List.map (fun (w, t) -> {block with text=t; cell_width=w}) chunks) @ acc;
    end [] paragraph)
  end paragraphs
;;

(** struct_by_lines *)
let struct_by_lines paragraphs =
  List.map begin fun lines ->
    List.map begin fun line ->
      let line_width = List.fold_left (fun acc c -> acc +. c.cell_width) 0.0 line in
      {line_width = line_width; line_height = 0.0; line_cells = line}
    end lines
  end paragraphs;
;;

(** struct_by_paragraphs *)
let struct_by_paragraphs paragraphs = List.map begin fun lines -> {
  paragraph_align        = 0.0;
  paragraph_line_spacing = 0.0;
  paragraph_lines        = lines;
} end paragraphs
;;

(** set_line_numbers *)
let set_line_numbers ~paragraphs ~avail_width =
  let current_line = ref 0 in
  let current_width = ref 0.0 in
  List.iter begin fun paragraph ->
    List.iter begin fun cell ->
      if !current_width +. cell.cell_width >= avail_width then begin
        current_width := cell.cell_width;
        incr current_line;
      end else begin
        current_width := !current_width +. cell.cell_width
      end;
      cell.line <- !current_line;
    end paragraph
  end paragraphs
;;

(** set_paragraph_align *)
let set_paragraph_align paragraphs =
  List.iter begin fun par ->
    let align =
      List.fold_left begin fun acc line ->
        max acc (List.fold_left (fun acc x -> max acc x.attr.align) 0.0 line.line_cells)
      end 0.0 par.paragraph_lines
    in
    par.paragraph_align <- align;
  end paragraphs
;;

(** set_paragraph_line_spacing *)
let set_paragraph_line_spacing paragraphs =
  let overall_height = ref 0.0 in
  List.iter begin fun par ->
    let line_spacing =
      List.fold_left begin fun acc line ->
        max acc (List.fold_left (fun acc x -> max acc x.attr.line_spacing) 0.0 line.line_cells)
      end 0.0 par.paragraph_lines
    in
    par.paragraph_line_spacing <- line_spacing;
    List.iter begin fun line ->
      line.line_height <- List.fold_left (fun acc c -> max acc c.cell_height) 0.0 line.line_cells;
      overall_height := !overall_height +. line.line_height +. line_spacing;
    end par.paragraph_lines
  end paragraphs;
  !overall_height
;;

(** draw_underline *)
let draw_underline ~x ~y ~cell doc =
  let scale = PDF.scale doc in
  let old = PDF.line_width doc in
  let r, g, b = PDF.draw_color doc in
  let fs = PDF.font_size doc in
  let uw = fs /. 64. in
  PDF.set_line_width uw doc;
  let red, green, blue = PDF.text_color doc in
  PDF.set_draw_color ~red ~green ~blue doc ;
  match cell.attr.underline with
    | `NONE -> ()
    | `SINGLE ->
      let y = y +. (fs /. scale) *. 0.38 in
      PDF.line
        ~x1:(x +. uw /. 2.) ~y1:y
        ~x2:(x +. cell.cell_width -. uw /. 2.) ~y2:y doc;
    | `LOW ->
      let y = y +. (fs /. scale) *. 0.55 in
      PDF.line
        ~x1:(x +. uw /. 2.) ~y1:y
        ~x2:(x +. cell.cell_width -. uw /. 2.) ~y2:y doc;
  PDF.set_line_width old doc;
  PDF.set_draw_color ~red:r ~green:g ~blue:b doc
;;

(** analyze *)
let analyze ~x ~y ~width ~markup ?(padding=(0., 0., 0., 0.)) ?(border_width=0.) doc =
  let pad_top, pad_right, pad_bottom, pad_left = padding in
  let avail_width = width -. pad_left -. pad_right -. 2. *. border_width in
  let blocks = markup_to_blocks ~markup doc in
  let paragraphs = group_by (fun x y -> x.par = y.par) blocks in
  let paragraphs = split_blocks_at_line_break ~paragraphs ~avail_width doc in
  set_line_numbers ~paragraphs ~avail_width;
  let paragraphs = List.map (group_by (fun x y -> x.line = y.line)) paragraphs in
  let paragraphs = struct_by_lines paragraphs in
  let paragraphs = struct_by_paragraphs paragraphs in
  set_paragraph_align paragraphs;
  let overall_height = set_paragraph_line_spacing paragraphs in
  (*(* Print debug info *)
  List.iter begin fun {paragraph_lines=lines} ->
    List.iter begin function {line_width=line_width; line_cells=cells} ->
      List.iter begin fun cell ->
        Printf.printf "%5.2f {w=%6.2f; h=%5.2f; color=% 7s; l=%2d; p=%2d; text=%S}\n%!"
          line_width
          cell.cell_width cell.cell_height
          cell.attr.color cell.line cell.par cell.text;
      end cells;
      (*Printf.printf "\n%!" ;*)
    end lines;
    Printf.printf "======== %5.2f ========\n%!" width0;
  end paragraphs;*)
  (* Return analysis *)
  {
    width      = width;
    height     = (overall_height +. pad_top +. pad_bottom +. 2. *. border_width);
    paragraphs = paragraphs
  }
;;

(** print_text *)
let print_text ~x ~y ~width ~analysis ?(padding=(0., 0., 0., 0.)) ?(border_width=0.) doc =
  let pad_top, pad_right, pad_bottom, pad_left = padding in
  let x0 = x +. pad_left +. border_width in
  let y0 = y +. pad_top +. border_width -. begin
    match analysis.paragraphs with
      | par :: _ ->
        begin
          match par.paragraph_lines with
            | line :: _ -> line.line_height /. 2.
            | _ -> 0.
        end;
      | _ -> 0.
  end in
  let x = ref x0 in
  let y = ref y0 in
  List.iter begin function {
    paragraph_align        = paragraph_align;
    paragraph_line_spacing = paragraph_line_spacing;
    paragraph_lines        = lines
  } ->
    List.iter begin function {line_width=line_width; line_height=line_height; line_cells=cells} ->
      x := x0 +. (width -. line_width) *. paragraph_align;
      y := !y +. line_height +. paragraph_line_spacing;
      List.iter begin fun cell ->
        if cell.cell_width > 0.0 then begin
          let text = cell.text in
          let red, green, blue = PDFUtil.rgb_of_hex cell.attr.color in
          PDF.set_text_color ~red ~green ~blue doc;
          let old_font_scale = PDF.font_scale doc in
          let hscaling = match cell.attr.scale with Some z -> Some (int_of_float (z *. 100.)) | _ -> None in
          PDF.set_font ~family:cell.attr.family ~style:cell.attr.style
            ~size:cell.attr.size ?scale:hscaling doc;
          PDF.set ~x:!x ~y:!y doc;
          PDF.cell ~width:cell.cell_width ~fill:false ~padding:0. ~text (*~height:3.0 ~border:[`All]*) doc;
          PDF.set_font ?scale:old_font_scale doc;
          draw_underline ~x:!x ~y:!y ~cell doc;
          x := !x +. cell.cell_width;
        end;
      end cells;
    end lines;
  end analysis.paragraphs;
;;

(** print *)
let print ~x ~y ~width ?height ~markup ?bgcolor ?border_width ?border_color ?(border_radius=0.) ?(padding=(0., 0., 0., 0.)) ?(valign=0.0) (*?pre*) doc =
  let analysis = analyze ~x ~y ~width ~markup ~padding ?border_width doc in
  let width = analysis.width in
  let old_text_color = PDF.text_color doc in
  let old_bgcolor = PDF.fill_color doc in
  let old_draw_color = PDF.draw_color doc in
  let old_line_width = PDF.line_width doc in
  let old_font_family = PDF.font_family doc in
  let old_font_size = PDF.font_size doc in
  let old_font_style = PDF.font_style doc in
  let old_font_scale = PDF.font_scale doc in
  let y = match height with Some h (*when h >= analysis.height*) -> y +. (h -. analysis.height) *. valign | _ -> y in
  begin
    (match bgcolor with None -> () | Some bgcolor -> let red, green, blue = PDFUtil.rgb_of_hex bgcolor in PDF.set_fill_color ~red ~green ~blue doc);
    (match border_color with None -> () | Some color -> let red, green, blue = PDFUtil.rgb_of_hex color in PDF.set_draw_color ~red ~green ~blue doc);
    (match border_width with None -> () | Some line_width -> PDF.set_line_width line_width doc);
    let style = match bgcolor, (border_color, border_width) with
      | None, (None, None) -> None
      | (Some _), (None, None) -> Some `Fill
      | None, (a, b) when a <> None || b <> None -> Some `Outline
      | (Some _), (a, b) when a <> None || b <> None -> Some `Both
      | _ -> assert false
    in
    (match style with None -> () | Some style ->
      let bw = match border_width with None -> 0. | Some x -> x in
      (*let height = match height with Some h -> h | _ -> analysis.height in*)
      PDF.rect ~x:(x +. bw /. 2.) ~y ~radius:border_radius
        ~width:(width -. bw)
        ~height:(analysis.height -. bw) ~style doc);
  end;
  (*(match pre with Some f -> f analysis | _ -> ());*)
  print_text ~x ~y ~width ~padding ~analysis ?border_width doc;
  let red, green, blue = old_text_color in PDF.set_text_color ~red ~green ~blue doc;
  let red, green, blue = old_bgcolor in PDF.set_fill_color ~red ~green ~blue doc;
  let red, green, blue = old_draw_color in PDF.set_draw_color ~red ~green ~blue doc;
  PDF.set_font ?family:old_font_family ~size:old_font_size ?scale:old_font_scale ~style:old_font_style doc;
  PDF.set_line_width old_line_width doc;
  analysis.width, analysis.height
;;





