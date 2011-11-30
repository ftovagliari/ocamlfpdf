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

type t = {
  mutable family    : Font.family option;
  mutable style     : Font.style list option;
  mutable size      : float option;
  mutable underline : [`NONE | `SINGLE | `LOW ];
  mutable align     : float;
  mutable color     : string option;
  mutable bgcolor   : string option;
  mutable fill      : bool;
}

type analysis = {
  mutable an_lines_height      : (int * float) list;
  mutable an_lines_avail_width : (int * float) list;
}

exception Break_char of int * int
exception Break_line

let find_word_bound_backward =
  let blanks = Str.regexp "[ \r\n\t]" in
  let non_blanks = Str.regexp "[^ ]" in
  fun bound text i ->
    let i = if i < 0 then 0 else i in
    match bound with
      | `START ->
        let stop = try Str.search_backward non_blanks text i with Not_found -> i in
        (try (Str.search_backward blanks text stop) + 1 with Not_found -> 0)
      | `STOP ->
        let start = try Str.search_backward blanks text i with Not_found -> i in
        (try (Str.search_backward non_blanks text start) + 1 with Not_found -> 0)
;;

(*#load "str.cma";;
let s = "a    symbols_are_set_in_typewriter_font (like this).  Non-terminal are set in_italic ";;
Str.search_backward (Str.regexp "[ \r\n\t]") s 16 + 1;;
Str.search_backward (Str.regexp "[^ ]") s 4;;
let x = find_word_bound_backward `START s 16;;
let x = find_word_bound_backward `STOP s 16;;
find_word_bound_backward `STOP s (x - 1);;*)

let rgb_of_hex name = Scanf.sscanf name "#%2x%2x%2x" (fun r g b -> (r, g, b));;

let hex_of_rgb (r, g, b) = Printf.sprintf "#%02X%02X%02X" r g b;;

let underline_of_string name =
  let name = String.lowercase name in
  match name with
    | "none" -> `NONE
    | "single" -> `SINGLE
    | "low" -> `LOW
    | _ -> invalid_arg "PDFMarkup.underline_of_string";;

let split_attrib = let re = Str.regexp "[,;][ ]*" in Str.split re

(** print' *)
let print' ~x ~y ~width ~markup ?(align=`Left) ?(padding=0.) ?(border_width=0.)
    ?(analyze=false)
    ?(analysis={an_lines_height=[]; an_lines_avail_width=[]})
    doc =
  let scale = PDF.scale doc in
  let markup = Str.global_replace (Str.regexp "\n") "<BR/>" markup in
  let xml = Xml.parse_string ("<MARKUP>" ^ markup ^ "</MARKUP>") in
  (*if analyze then (Printf.printf "%s\n%!" (Xml.to_string_fmt xml));*)
  (** Globals *)
  let x0 = x +. padding +. border_width /. 2. in
  let y0 = y +. padding +. border_width /. 2. in
  let width0 = width -. 2. *. padding -. 2. *. border_width in
  let fill = ref false in
  let underline = ref `NONE in
  let remaining = ref None in
  let avail_width = ref width0 in
  let avail_width_exceeded = ref false in
  let current_line = ref 0 in
  let get_current_line_height () =
    try let clh = List.assoc !current_line analysis.an_lines_height in clh
    with Not_found -> PDF.font_size doc /. scale in
  let add_current_line_height is_new_line =
    if analyze then begin
      let lh =
        if is_new_line then get_current_line_height()
        else max (PDF.font_size doc /. scale) (get_current_line_height())
      in
      analysis.an_lines_height <- (!current_line, lh) :: (List.remove_assoc !current_line analysis.an_lines_height);
    end;
  in
  (** default_style *)
  let default_style = {
    family    = (PDF.font_family doc);
    style     = Some [];
    size      = Some (PDF.font_size doc);
    underline = `NONE;
    align     = 0.0;
    color     = Some (hex_of_rgb (PDF.text_color doc));
    bgcolor   = None;
    fill      = false;
  } in
  let current_attribs = ref default_style in
  let current_paragraph = ref 0 in
  let x = ref x0 in
  let y = ref y0 in
  let newline () =
    let clh = get_current_line_height() in
    y := !y +. clh;
    let offset =
      if not analyze then begin
        let align = !current_attribs.align in
        let law = try List.assoc !current_line analysis.an_lines_avail_width with Not_found -> 0.0 in
        law *. align;
      end else 0.0
    in
    x := x0 +. offset;
  in
  (** char_width *)
  let cw ~family ~style =
    let key = Font.key_of_font style family in
    Font.get_metric key
  in
  let char_width = ref (fun x -> 0.0) in
  (** set_attrib *)
  let set_attrib attribs =
    let family = if attribs.family = None then default_style.family else attribs.family in
    let style = if attribs.style = None then default_style.style else attribs.style in
    let size = if attribs.size = None then default_style.size else attribs.size in
    underline := attribs.underline;
    PDF.set_font ?family ?style ?size doc;
    let color = if attribs.color = None then default_style.color else attribs.color in
    (match color with None -> () | Some x -> let red, green, blue = rgb_of_hex x in PDF.set_text_color ~red ~green ~blue doc);
    let bgcolor = if attribs.bgcolor = None then default_style.bgcolor else attribs.bgcolor in
    (match bgcolor with None -> fill := false | Some x -> let red, green, blue = rgb_of_hex x in PDF.set_fill_color ~red ~green ~blue doc; fill := true);
    let family = match PDF.font_family doc with Some x -> x | None -> `Courier in
    let f = cw ~family ~style:(PDF.font_style doc) in
    char_width := (fun x -> ((float (f x)) *. (PDF.font_size doc) /. 1000.) /. scale);
    current_attribs := attribs;
  in
  (** print_text *)
  let rec print_text text =
    let i = ref 0 in
    let text = ref text in
    if !text <> "" then begin
      let consumed_width = ref 0. in
      let new_line_pending = ref false in
      (** print_cell *)
      let print_cell stop =
        let len = String.length !text in
        let remaining_text = String.sub !text !i (len - !i) in
        let width_is_exact = !avail_width_exceeded && remaining_text = "" in
        remaining :=
          if (not !new_line_pending) && (not !avail_width_exceeded || width_is_exact || remaining_text = "") then None
          else (Some (remaining_text));
        let is_new_line = !remaining <> None || width_is_exact in
        let text = String.sub !text 0 stop (*!i*) in
        add_current_line_height is_new_line;
        if not analyze then begin
          if !current_line = 0 && !y = y0 then (newline());
          PDF.set ~x:!x ~y:!y doc;
          let width = !consumed_width in
          PDF.cell ~width ~fill:!fill (*~height:3. ~border:[`R;`L]*) ~padding:0. ~text (*~align:`Left*) doc;
          begin (* Draw underline *)
            let old = PDF.line_width doc in
            let r, g, b = PDF.draw_color doc in
            let fs = PDF.font_size doc in
            let uw = fs /. 64. in
            PDF.set_line_width uw doc;
            let red, green, blue = PDF.text_color doc in
            PDF.set_draw_color ~red ~green ~blue doc ;
            match !underline with
              | `NONE -> ()
              | `SINGLE ->
                let y = !y +. uw *. 3.5 in
                PDF.line
                  ~x1:(!x +. uw /. 2.) ~y1:y
                  ~x2:(!x +. !consumed_width -. uw /. 2.) ~y2:y doc;
              | `LOW ->
                let y = !y +. (fs /. scale) /. 4. in
                PDF.line
                  ~x1:(!x +. uw /. 2.) ~y1:y
                  ~x2:(!x +. !consumed_width -. uw /. 2.) ~y2:y doc;
            PDF.set_line_width old doc;
            PDF.set_draw_color ~red:r ~green:g ~blue:b doc ;
          end
        end;
        x := !x +. !consumed_width;
        (* Move to a new line, if necessary. *)
        if is_new_line then begin
          avail_width := width0;
          incr current_line;
          newline();
        end;
      in
      (** Splits text in writeable cells. *)
      begin
        let find_line_break () =
          let prev_start = find_word_bound_backward `START !text !i in
          let prev_stop = find_word_bound_backward `STOP !text (prev_start - 1) in
          if analyze then begin
            if !avail_width_exceeded then
              for j = prev_stop to !i do
                let ch = String.unsafe_get !text j in
                avail_width := !avail_width +. (!char_width ch);
              done;
            (*Printf.printf "#### %2d: i=%2d prev=(%2d,%2d) avail_width=%5.2f align=%2.1f exceed=%b par=%d%!"
              !current_line !i prev_stop prev_start !avail_width !current_attribs.align !avail_width_exceeded !current_paragraph;
            Printf.printf " %S\n%!" (String.sub !text prev_stop (min (String.length !text - prev_stop) (!i - prev_stop + 1)));*)
            analysis.an_lines_avail_width <- (!current_line, !avail_width) :: analysis.an_lines_avail_width;
          end else begin
            if !avail_width_exceeded && prev_start > 0 then
              for j = prev_stop to !i - 1 do
                let ch = String.unsafe_get !text j in
                consumed_width := !consumed_width -. (!char_width ch);
              done;
          end;
          prev_stop, prev_start;
        in
        let pending_space_width = ref 0.0 in
        try
          avail_width_exceeded := false;
          let len = String.length !text in
          while not !avail_width_exceeded && !i < len do
            let ch = String.unsafe_get !text !i in
            if ch = '\n' then begin
              new_line_pending := true;
              incr i;
              raise Break_line
            end;
            let cw = !char_width ch in
            if ch = ' ' then (pending_space_width := !pending_space_width +. cw)
            else begin
              avail_width := !avail_width -. !pending_space_width -. cw;
              if !avail_width > 0. then (consumed_width := !consumed_width +. !pending_space_width +. cw)
              else begin
                avail_width_exceeded := true;
                let stop, start = find_line_break() in
                if start = 0 then begin
                  raise (Break_char (start, stop));
                end;
              end;
              pending_space_width := 0.0;
            end;
            incr i;
          done;
          raise Break_line;
        with
          | Break_char (start, stop) ->
            (*eprintf "Break_char %S i=%d stop=%d start=%d consumed_width=%5.2f\n%!" !text !i stop start !consumed_width;*)
            analysis.an_lines_avail_width <- (!current_line, 0.0) :: analysis.an_lines_avail_width;
            print_cell !i;
          | Break_line -> begin
            consumed_width := !consumed_width +. !pending_space_width;
            let _, start =
              if not !avail_width_exceeded then find_line_break()
              else 0, (find_word_bound_backward `START !text !i)
            in
            if !i < String.length !text (*- 1*) then begin
              i := start;
            end;
            let stop = if !avail_width_exceeded then find_word_bound_backward `STOP !text !i else !i in
            print_cell stop;
          end;
      end;
    end;
    print_remaining_text ();
  and print_remaining_text () =
    while !remaining <> None do
      match !remaining with
        | Some text -> print_text text;
        | _ -> assert false
    done;
  in
  (** Markup traversal *)
  Xml.iter begin function
    | Xml.PCData "&nbsp;" ->
      set_attrib default_style;
      print_text " ";
    | Xml.PCData text ->
      set_attrib default_style;
      print_text text;
    | Xml.Element (tag, _, _) when (String.lowercase tag) = "br" ->
      avail_width := width0;
      incr current_line;
      add_current_line_height true;
      newline();
      incr current_paragraph;
    | Xml.Element (tag, attrs, children) when (String.lowercase tag) = "span" ->
      let attr = {
        family    = (try Some (Font.family_of_string (List.assoc "family" attrs)) with Not_found -> None);
        style     = (try Some (List.map Font.style_of_string (split_attrib (List.assoc "style" attrs))) with Not_found -> None);
        size      = (try Some (float_of_string (List.assoc "size" attrs)) with Not_found -> None);
        underline = (try underline_of_string (List.assoc "underline" attrs) with Not_found -> `NONE);
        align     = (try float_of_string (List.assoc "align" attrs) with Not_found -> 0.0);
        color     = (try Some (List.assoc "color" attrs) with Not_found -> None);
        bgcolor   = (try Some (List.assoc "bgcolor" attrs) with Not_found -> None);
        fill      = false; (* dummy *)
      } in
      set_attrib attr;
      begin
        match children with
          | [] ->
            print_text " ";
          | children ->
            List.iter begin function
              | Xml.PCData text ->
                print_text text;
              | Xml.Element (tag, _, _) when (String.lowercase tag) = "br" ->
                avail_width := width0;
                incr current_line;
                add_current_line_height true;
                newline();
                incr current_paragraph;
              | _ -> failwith "invalid_markup"
            end children
      end;
    | Xml.Element (tag, _, []) ->
      set_attrib default_style;
      print_text "";
    | _ -> failwith "invalid_markup"
  end xml;
  set_attrib default_style;
  let text_height = List.fold_left (fun acc (_, lh) -> acc +. lh) 0. analysis.an_lines_height in
  (** Return the actual width and height *)
  width0 +. 2. *. (padding +. border_width),
  text_height +. 2. *. padding +. border_width,
  analysis

(** print *)
let print ~x ~y ~width ~markup ?bgcolor ?border_width ?border_color ?(border_radius=0.) ?(padding=0.) doc =
  let width, height, analysis =
    print' ~x ~y ~width ~markup ~padding ~analyze:true ?border_width doc
  in
  let old_bgcolor = PDF.fill_color doc in
  let old_draw_color = PDF.draw_color doc in
  let old_line_width = PDF.line_width doc in
  begin
    (match bgcolor with None -> () | Some bgcolor -> let red, green, blue = rgb_of_hex bgcolor in PDF.set_fill_color ~red ~green ~blue doc);
    (match border_color with None -> () | Some color -> let red, green, blue = rgb_of_hex color in PDF.set_draw_color ~red ~green ~blue doc);
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
      PDF.rect ~x:(x +. bw /. 2.) ~y ~radius:border_radius
        ~width:(width -. 1. *. bw)
        ~height ~style doc);
  end;
  let red, green, blue = old_bgcolor in PDF.set_fill_color ~red ~green ~blue doc;
  let red, green, blue = old_draw_color in PDF.set_draw_color ~red ~green ~blue doc;
  PDF.set_line_width old_line_width doc;
  let w, h, _ = print' ~x ~y ~width ~markup ~padding ~analyze:false ~analysis ?border_width doc in
  w, h






