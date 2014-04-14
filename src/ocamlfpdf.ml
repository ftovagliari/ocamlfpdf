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
open Font
open Fpdf_document

let (//) = Filename.concat

let normalize =
  let re_amp  = Str.regexp "&" in
  let re_lt   = Str.regexp "<" in
  let re_gt   = Str.regexp ">" in
  let re_apos = Str.regexp "'" in
  let re_quot = Str.regexp "\"" in
  function markup ->
    let markup = Str.global_replace re_amp "&amp;" markup in
    let markup = Str.global_replace re_lt "&lt;" markup in
    let markup = Str.global_replace re_gt "&gt;" markup in
    let markup = Str.global_replace re_apos "&apos;" markup in
    let markup = Str.global_replace re_quot "&quot;" markup in
    markup

let split_spaces =
  let re = Str.regexp " +" in
  Str.split re

let markup_of_xml xml = Xml.fold (fun acc x -> acc ^ (Xml.to_string x)) "" xml

let bool_attr ~default node name =
  try bool_of_string (Xml.attrib node name)
  with Xml.No_attribute _ | Xml.Not_element _ | Failure "bool_of_string" -> default

let float_attr ~default node name =
  try float_of_string (Xml.attrib node name)
  with Xml.No_attribute _ | Xml.Not_element _ | Failure "float_of_string" -> default

let int_attr ~default node name =
  try int_of_string (Xml.attrib node name)
  with Xml.No_attribute _ | Xml.Not_element _ | Failure "int_of_string" -> default

let parse4 xml name f =
  match List.map float_of_string (split_spaces (Xml.attrib xml name)) with
    | [a; b; c; d] -> f a b c d
    | _ -> ()

let parse4t xml name =
  let r = ref (0.0, 0.0, 0.0, 0.0) in
  (try parse4 xml name (fun a b c d -> r := a, b, c, d) with Xml.No_attribute _ -> ());
  !r


let attrib_default xml name default =
  try Xml.attrib xml name with Xml.No_attribute _ -> default

(** main *)
let main () = begin
  let filename = Sys.argv.(1) in
  let open_pdf = try Sys.argv.(2) = "-a" with _ -> false in
  let outchan = stdout in
  let close_file () = ()
(*    if Sys.file_exists filename then Sys.remove filename*)
  in
  begin
    try
      let doc               = Fpdf.create ~outchan () in
      let scale             = Fpdf.scale doc in
      Fpdf.set_auto_page_break false doc;
      Fpdf.set_line_cap `Round doc;
      Fpdf.set_line_width 0.25 doc;

(*      Fpdf_font.embed_font ~family:`CenturySchoolbook ~style:[] doc;
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
      Fpdf_font.embed_font ~family:`CMUSansSerif_DemiCondensed ~style:[] doc;*)


      let width_avail = ref (Fpdf_page.page_width doc) in
      let height_avail = ref (Fpdf_page.page_height doc) in
      let x0 = ref 0.0 in
      let y0 = ref 0.0 in
      let current_x = ref !x0 in
      let current_y = ref !y0 in
      let y_max = ref (!y0 +. !height_avail) in
      let create_page () =
        Fpdf.add_page doc;
        (*if Fpdf.page_count doc > 0 then begin
          Fpdf.push_graphics_state doc;
          Fpdf.set_line_width 0.5 doc;
          Fpdf.set_draw_color ~red:255 ~green:0 ~blue:0 doc;
          Fpdf.rect ~x:!x0 ~y:!y0 ~width:!width_avail ~height:!height_avail doc;
          Fpdf.pop_graphics_state doc;
        end;*)
        current_x := !x0;
        current_y := !y0;
      in

      let eval filename =
        let xml = Xml.parse_file filename in
        match Xml.tag xml with
          | "DOCUMENT" ->
            (* author *)
            Fpdf.set_author (attrib_default xml "author" "") doc;
            Fpdf.set_creator (attrib_default xml "creator" (Filename.basename Sys.executable_name)) doc;
            Fpdf.set_creation_date (attrib_default xml "creation-date" "") doc;
            Fpdf.set_title (attrib_default xml "title" "") doc;
            Fpdf.set_subject (attrib_default xml "subject" "") doc;
            (* Margin *)
            parse4 xml "margin" begin fun top right bottom left ->
              Fpdf.set_margins ~top ~right ~bottom ~left doc;
              width_avail := Fpdf_page.page_width doc -.  left -. right;
              height_avail := Fpdf_page.page_height doc -. top -. bottom;
              x0 := left;
              y0 := top;
              y_max := !y0 +. !height_avail;
            end;
            (* Font *)
            let family = try Some (Font.family_of_string (Xml.attrib xml "font-family")) with Xml.No_attribute _ -> None in
            let size = try Some (float_of_string (Xml.attrib xml "font-size")) with Xml.No_attribute _ | Failure "float_of_string" -> None in
            Fpdf.set_font ?family ?size doc;
            Xml.iter begin fun node ->
              match Xml.tag node with
                | "PAGE" -> create_page()
                | "MARKUP" ->
                  let (*(pt, pr, pb, pl) as *)padding = parse4t node "padding" in
                  let analysis =
                    Fpdf_markup.prepare
                      ~width:!width_avail ~markup:(markup_of_xml node)
                      (*~border_width:(Fpdf.line_width doc)*)
                      ~padding
                      doc
                  in
                  let x = ref (float_attr ~default:!current_x node "x") in
                  let y = ref (float_attr ~default:!current_y node "y") in
                  if !y +. analysis.Fpdf_markup.height > !y_max && bool_attr ~default:false node "auto_page_break"
                  then begin
                    create_page ();
                    y := !current_y;
                  end;
                  analysis.Fpdf_markup.print ~x:!x ~y:!y ();
                  current_y := !y +. analysis.Fpdf_markup.height;
                | "TABULAR" ->
                  if !current_y > !y_max then create_page();
                  let debug = bool_attr ~default:false node "debug" in
                  let colwidths = Array.of_list (List.map float_of_string (split_spaces (Xml.attrib node "colwidths"))) in
                  let xt = float_attr ~default:!current_x node "x" in
                  let yt = float_attr ~default:!current_y node "y" in
                  let tabular = Fpdf_tabular.create
                      ~x:xt ~y:yt
                      ~border_width:(`Size 0.0)
                      ~width:!width_avail
                      ~colwidths
                      ~debug doc
                  in
                  let last_h = ref 0. in
                  let count_table_pages = ref 0 in
                  let current_table_height = ref 0.0 in
                  let set = Fpdf_tabular.set_markup ~yalign:0.0 tabular in
                  Xml.iter begin fun node ->
                    match Xml.tag node with
                      | "SET" | "CELL" ->
                        let row = int_attr ~default:0 node "row" in
                        let col = int_attr ~default:0 node "col" in
                        let rowspan = int_attr ~default:1 node "rowspan" in
                        let colspan = int_attr ~default:1 node "colcol" in
                        Xml.iter begin fun node ->
                          match Xml.tag node with
                            | "MARKUP" -> set ~rowspan ~colspan row col (markup_of_xml node);
                            | _ -> failwith "MARKUP expected"
                        end node;
                      | _ -> failwith "CELL expected"
                  end node;
                  let matrix = Fpdf_tabular.build_matrix tabular in
                  Fpdf_tabular.iter_rows matrix begin fun i _ ->
                    let origin = if !count_table_pages = 0 then yt else !y0 in
                    let current_row_height = Fpdf_tabular.row_height tabular i in
                    current_table_height := !current_table_height +. current_row_height;
                    let cur_height_in_page = !current_table_height -. !last_h in
                    let yy = origin +. cur_height_in_page in
                    if yy > !y_max then begin
                      incr count_table_pages;
                      last_h := !last_h +. cur_height_in_page -. current_row_height;
                      Fpdf_tabular.add_page_break_before i tabular;
                    end;
                    current_y := yy;
                  end;
                  Fpdf_tabular.pack ~matrix tabular;
                | x -> kprintf failwith "parse %S" x
            end xml
          | _ -> failwith "DOCUMENT expected"
      in
      eval filename;

      (**  *)
      Fpdf.close_document doc;
      close_file();
      if open_pdf then Sys.command filename |> ignore;
    with
      | (Xml.Not_element xml) as ex ->
        let src = Fpdf_util.fread filename in
        close_file();
        Printf.eprintf "%s\n\n%!" src;
        kprintf failwith "%s\n%s" (Printexc.to_string ex) (Xml.to_string_fmt xml)
      | Xml.Error (msg, pos) ->
        let src = Fpdf_util.fread filename in
        close_file();
        let line = Xml.line pos in
        let start, stop = Xml.range pos in
        raise (Fpdf_error.Error
                 (Fpdf_error.Invalid_markup,
                  (sprintf "Error line %d, characters %d-%d: %s\n%s" line start stop (Xml.error_msg msg) src)))
      | ex ->
        close_file();
        raise ex
  end;
end

let _ = Fpdf_error.handle_error main ()
