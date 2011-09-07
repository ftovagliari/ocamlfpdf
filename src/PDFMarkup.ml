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
let print' ~x ~y ~width ~line_height ~markup ?(align=`Left) ?(padding=0.) ?(print=true) ?(border_width=0.) doc =
  let if_print f x = if print then (f x) else () in
  let markup = Str.global_replace (Str.regexp "\n\n") "<BR/><BR/>" markup in
  let xml = Xml.parse_string ("<MARKUP>" ^ markup ^ "</MARKUP>") in
  (** Globals *)
  let x0 = x +. padding +. border_width in
  let y0 = y +. padding +. border_width /. 2. in
  let width0 = width -. 2. *. padding -. 2. *. border_width in
  let fill = ref false in
  let underline = ref `NONE in
  let remaining = ref None in
  let avail_width = ref width0 in
  let x = ref x0 in
  let y = ref y0 in
  (** default_style *)
  let default_style = {
    family = (PDF.font_family doc);
    style = Some (PDF.font_style doc);
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
    char_width := fun x -> ((float (f x)) *. (PDF.font_size doc) /. 1000.) /. (PDF.scale doc)
  in
  (** write_text *)
  let write_text ~x ~y ~text ~avail_width =
    let i = ref 0 in
    let text = if !x = x0 then PDFUtil.ltrim text else text in
    if (*PDFUtil.trim*) text <> "" then begin
      let consumed_width = ref 0. in
      let new_line = ref false in
      begin
        try
          let len = String.length text in
          while !avail_width > 0. && !i < len do
            let ch = String.unsafe_get text !i in
            if ch = '\n' then (new_line := true; (*consumed_width := !consumed_width +. cw; *)incr i; raise Exit);
            let cw = !char_width ch in
            avail_width := !avail_width -. cw;
            if !avail_width > 0. then (consumed_width := !consumed_width +. cw);
            incr i;
          done;
          raise Exit;
        with Exit -> begin
          i := prev_word_start text !i;
          let remaining_text = String.sub text !i (String.length text - !i) in
          let width_is_exact = !avail_width = 0. && remaining_text = "" in
          remaining :=
            if (not !new_line) && (!avail_width > 0. || width_is_exact || remaining_text = "") then None
            else (Some (remaining_text));
          let is_new_line = !remaining <> None || width_is_exact in
          let text = String.sub text 0 !i in
          if_print (PDF.set ~x:!x ~y:!y) doc;
          if !consumed_width = 0. then (consumed_width := 0.);
          if_print (PDF.cell ~fill:!fill ~width:!consumed_width ~height:line_height ~border:[] ~padding:0. ~text) doc;
          let old = PDF.line_width doc in
          begin
            let uw = (*min (0.5 /. (PDF.scale doc))*) (PDF.font_size doc /. 64.) in
            if_print (PDF.set_line_width uw) doc;
            match !underline with
              | `NONE -> ()
              | `SINGLE ->
                let y = !y +. 0.75 *. line_height +. 3. /. 2. *. uw in
                if_print (PDF.line ~x1:(!x +. uw /. 2.) ~y1:y ~x2:(!x +. !consumed_width -. uw /. 2.) ~y2:y) doc;
              | `LOW ->
                let y = !y +. 0.75 *. line_height +. ((PDF.font_size doc) /. (PDF.scale doc)) /. 4. (*+. uw /. 2.*) in
                if_print (PDF.line ~x1:(!x +. uw /. 2.) ~y1:y ~x2:(!x +. !consumed_width -. uw /. 2.) ~y2:y) doc;
          end;
          if_print (PDF.set_line_width old) doc;
          x := !x +. !consumed_width;
          (* Move to a new line *)
          if is_new_line then begin
            avail_width := width0;
            y := !y +. line_height;
            x := x0;
          end;
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
    | Xml.Element (tag, attrs, [Xml.PCData text]) when (String.lowercase tag) = "span" ->
      let style = {
        family = (try Some (Font.family_of_string (List.assoc "family" attrs)) with Not_found -> None);
        style = (try Some (List.map Font.style_of_string (split_attrib (List.assoc "style" attrs))) with Not_found -> None);
        size = (try Some (float_of_string (List.assoc "size" attrs)) with Not_found -> None);
        underline = (try underline_of_string (List.assoc "underline" attrs) with Not_found -> `NONE);
        color = (try Some (List.assoc "color" attrs) with Not_found -> None);
        bgcolor = (try Some (List.assoc "bgcolor" attrs) with Not_found -> None);
        fill = false; (* dummy *)
      } in
      write_remaining_text ();
      set_attrib style;
      write_text ~x ~y ~text ~avail_width;
    | Xml.Element (tag, _, _) when (String.lowercase tag) = "br" ->
      write_remaining_text ();
      avail_width := width0;
      y := !y +. line_height;
      x := x0;
    | Xml.Element (tag, _, []) ->
      write_remaining_text ();
      set_attrib default_style;
      write_text ~x ~y ~text:"" ~avail_width;
    | _ -> failwith "Invalid markup."
  end xml;
  write_remaining_text ();
  set_attrib default_style;
  width0 +. 2. *. (padding +. border_width), (!y -. y0) +. line_height +. 2. *. padding +. border_width

(** print *)
let print ~x ~y ~width ~line_height ~markup (*?(align=`Left)*) ?bgcolor ?border_width ?border_color ?(border_radius=0.) ?(padding=0.) doc =
  let width, height = print' ~x ~y ~width ~line_height ~markup (*~align*) ~padding ~print:false ?border_width doc in
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
  print' ~x ~y ~width ~line_height ~markup (*~align*) ~padding ~print:true ?border_width doc






