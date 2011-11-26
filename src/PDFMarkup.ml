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
  mutable family : Font.family option;
  mutable style : Font.style list option;
  mutable size : float option;
  mutable underline : [`NONE | `SINGLE | `LOW ];
  mutable color : string option;
  mutable bgcolor : string option;
  mutable fill : bool;
}

exception Break_char
exception Break_line

let blanks = [' '; '\n'; '\r'; '\t'];;

let prev_word_start text i =
  let len = String.length text in
  let rec loop i =
    if i >= 0 && i < len && (not (List.mem (String.unsafe_get text i) blanks)) then begin
      if i > 0 && (not (List.mem (String.unsafe_get text (i - 1)) blanks))
      then (loop (i - 1)) else i
    end else if i = len then i else (loop (i - 1))
  in loop i;;

let rgb_of_hex name = Scanf.sscanf name "#%2x%2x%2x" (fun r g b -> (r, g, b));;

let hex_of_rgb (r, g, b) = Printf.sprintf "#%02X%02X%02X" r g b;;

let underline_of_string name =
  let name = String.lowercase name in
  match name with
  | "none" -> `NONE
  | "single" -> `SINGLE
  | "low" -> `LOW
  | _ -> invalid_arg "underline_of_string"

let split_attrib = let re = Str.regexp "[,;][ ]*" in Str.split re

(** print' *)
let print' ~x ~y ~width ~markup ?(align=`Left) ?(padding=0.) ?(print=true) ?(line_heights=[]) ?(border_width=0.) doc =
  let scale = PDF.scale doc in
  let markup = Str.global_replace (Str.regexp "\n") "<BR/>" markup in
  let xml = Xml.parse_string ("<MARKUP>" ^ markup ^ "</MARKUP>") in
  (** Globals *)
  let x0 = x +. padding +. border_width /. 2. in
  let y0 = y +. padding +. border_width /. 2. in
  let width0 = width -. 2. *. padding -. 2. *. border_width in
  let fill = ref false in
  let underline = ref `NONE in
  let remaining = ref None in
  let avail_width = ref width0 in
  let line_heights = ref line_heights in
  let current_line = ref 0 in
  let get_current_line_height () = try List.assoc !current_line !line_heights
    with Not_found -> PDF.font_size doc /. scale in
  let add_current_line_height is_new_line =
    if not print then begin
      let lh =
        if is_new_line then get_current_line_height()
        else max (PDF.font_size doc /. scale) (get_current_line_height())
      in
      line_heights := (!current_line, lh) :: (List.remove_assoc !current_line !line_heights);
    end;
  in
  let x = ref x0 in
  let y = ref y0 in
  (** default_style *)
  let default_style = {
    family = (PDF.font_family doc);
    style = Some [] (*(PDF.font_style doc)*);
    size = Some (PDF.font_size doc);
    underline = `NONE;
    color = Some (hex_of_rgb (PDF.text_color doc));
    bgcolor = None;
    fill = false;
  } in
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
    char_width := fun x -> ((float (f x)) *. (PDF.font_size doc) /. 1000.) /. scale
  in
  (** write_text *)
  let write_text ~x ~y ~text ~avail_width =
    let i = ref 0 in
    let text = ref (if !x = x0 then PDFUtil.ltrim text else text) in
    if (*PDFUtil.trim*) !text <> "" then begin
      let consumed_width = ref 0. in
      let new_line = ref false in
      let last_newline = ref 0 in
      (** write_line: writes until a newline character or an attribute change. *)
      let write_line () =
        let remaining_text = String.sub !text !i (String.length !text - !i) in
        let width_is_exact = !avail_width = 0. && remaining_text = "" in
        remaining :=
          if (not !new_line) && (!avail_width > 0. || width_is_exact || remaining_text = "") then None
          else (Some (remaining_text));
        let is_new_line = !remaining <> None || width_is_exact in
        let text = String.sub !text 0 !i in
        add_current_line_height is_new_line;
        if print then begin
          let clh = get_current_line_height() in
          if !current_line = 0 && !y = y0 then begin
            y := !y +. clh;
          end;
          PDF.set ~x:!x ~y:!y doc;
          PDF.cell ~fill:!fill ~width:!consumed_width ~border:[] ~padding:0. ~text doc;
          let old = PDF.line_width doc in
          let r, g, b = PDF.draw_color doc in
          begin
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
          end;
          PDF.set_line_width old doc;
          PDF.set_draw_color ~red:r ~green:g ~blue:b doc ;
        end;
        x := !x +. !consumed_width;
        (* Move to a new line *)
        if is_new_line then begin
          avail_width := width0;
          incr current_line;
          y := !y +. get_current_line_height();
          x := x0;
        end;
      in
      (** Main loop of write_text *)
      begin
        try
          let len = String.length !text in
          while !avail_width > 0. && !i < len do
            let ch = String.unsafe_get !text !i in
            if ch = '\n' then begin
              new_line := true;
              (*consumed_width := !consumed_width +. cw; *)incr i;
              raise Break_line
            end;
            let cw = !char_width ch in
            avail_width := !avail_width -. cw;
            if !avail_width > 0. then (consumed_width := !consumed_width +. cw) else begin
              let prev_ws = prev_word_start !text !i in
              if prev_ws = !last_newline then begin
                new_line := true;
                text := (String.sub !text 0 !i) ^ "\n" ^ (String.sub !text !i (String.length !text - !i));
                incr i;
                raise Break_char;
              end
            end;
            incr i;
          done;
          incr i;
          raise Break_line;
        with
          | Break_char -> write_line()
          | Break_line -> begin
            i := prev_word_start !text !i;
            write_line();
        end;
      end;
    end
  in
  (** write_remaining_text *)
  let write_remaining_text () =
    while !remaining <> None do
      match !remaining with
        | None -> assert false
        | Some text ->
          write_text ~x ~y ~text ~avail_width;
    done;
  in
  (** Main loop *)
  Xml.iter begin function
    | Xml.PCData "&nbsp;" ->
      write_remaining_text ();
      set_attrib default_style;
      write_text ~x ~y ~text:" " ~avail_width;
    | Xml.PCData text ->
      write_remaining_text ();
      set_attrib default_style;
      write_text ~x ~y ~text ~avail_width;
    | Xml.Element (tag, _, _) when (String.lowercase tag) = "br" ->
      write_remaining_text ();
      avail_width := width0;
      incr current_line;
      add_current_line_height true;
      y := !y +. get_current_line_height();
      x := x0;
    | Xml.Element (tag, attrs, children) when (String.lowercase tag) = "span" ->
      let attr = {
        family    = (try Some (Font.family_of_string (List.assoc "family" attrs)) with Not_found -> None);
        style     = (try Some (List.map Font.style_of_string (split_attrib (List.assoc "style" attrs))) with Not_found -> None);
        size      = (try Some (float_of_string (List.assoc "size" attrs)) with Not_found -> None);
        underline = (try underline_of_string (List.assoc "underline" attrs) with Not_found -> `NONE);
        color     = (try Some (List.assoc "color" attrs) with Not_found -> None);
        bgcolor   = (try Some (List.assoc "bgcolor" attrs) with Not_found -> None);
        fill      = false; (* dummy *)
      } in
      write_remaining_text ();
      set_attrib attr;
      begin
        match children with
          | [] -> write_text ~x ~y ~text:" " ~avail_width;
          | children ->
            List.iter begin function
              | Xml.PCData text ->
                write_text ~x ~y ~text ~avail_width;
                write_remaining_text ();
              | Xml.Element (tag, _, _) when (String.lowercase tag) = "br" ->
                avail_width := width0;
                incr current_line;
                add_current_line_height true;
                y := !y +. get_current_line_height();
                x := x0;
              | _ -> failwith "invalid_markup"
            end children
      end;
    | Xml.Element (tag, _, []) ->
      write_remaining_text ();
      set_attrib default_style;
      write_text ~x ~y ~text:"" ~avail_width;
    | _ -> failwith "invalid_markup"
  end xml;
  write_remaining_text ();
  set_attrib default_style;
  let text_height = List.fold_left (fun acc (_, x) -> acc +. x) 0. !line_heights in
  (** Return the actual width and height *)
  width0 +. 2. *. (padding +. border_width),
  text_height +. 2. *. padding +. border_width,
  !line_heights

(** print *)
let print ~x ~y ~width ~markup (*?(align=`Left)*) ?bgcolor ?border_width ?border_color ?(border_radius=0.) ?(padding=0.) doc =
  let width, height, line_heights =
    print' ~x ~y ~width ~markup (*~align*) ~padding ~print:false ?border_width doc
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
  let w, h, _ = print' ~x ~y ~width ~markup (*~align*) ~padding ~print:true ~line_heights ?border_width doc in
  w, h






