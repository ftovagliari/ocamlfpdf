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
open Font_metrics
open PDFDocument

let (//) = Filename.concat
let main () = begin
  let filename = Sys.argv.(0) ^ ".pdf" in
  let outchan = open_out_bin filename in
  let close_file () = close_out outchan in
  begin
    try
      let doc               = PDF.create ~orientation:`Landscape ~outchan () in
      let margin            = 10.0 in
      let margin_top        = margin in
      let margin_right      = margin in
      let margin_bottom     = margin in
      let margin_left       = margin in
      let width_avail       = PDFPage.page_width doc -. margin_left -. margin_right in
      let height_avail      = PDFPage.page_height doc -. margin_top -. margin_bottom in
      let scale             = PDF.scale doc in
      let x0                = margin_left in
      let y0                = margin_top in
      let padding           = 3. in
      let label_font_size   = 8. in
      let label_font_family = `Helvetica in
      let label_font        = Font.find Font.Helvetica_Oblique in
      let font_size_pt      = (height_avail *. 0.47) *. scale in
      let x'                = width_avail *. 0.75 in
      let x''               = width_avail *. 0.85 in
      let gap               = label_font_size /. scale in

      PDF.set_margins ~top:margin_top ~right:margin_right ~bottom:margin_bottom ~left:margin_left doc;
      PDF.set_line_cap `Round doc;
      PDF.set_line_width 0.25 doc;

      (* print_vmesurements *)
      let print_vmesurements x vlines =
        let aw = 1. in
        let draw_marker = ref true in
        let last, _ =
          List.fold_left begin fun (prev, _) ((y, label) as p) ->
            begin
              match label with
                | Some text ->
                  let tw = PDF.get_text_width label_font label_font_size text /. scale in
                  let x = x0 +. x -. tw /. 2. in
                  let y = y -. 3. /. 2. *. gap in
                  PDF.set ~x ~y doc;
                  PDF.cell ~text ~font_family:label_font_family ~font_style:[`Italic] ~font_size:label_font_size ~border:[] doc;
                  draw_marker := false;
                | None ->
                  if !draw_marker then (PDF.line ~x1:(x0 +. x -. aw) ~y1:(prev +. aw) ~x2:(x0 +. x +. aw) ~y2:(prev -. aw) doc);
                  draw_marker := true;
                  PDF.line ~x1:(x0 +. x) ~y1:prev ~x2:(x0 +. x) ~y2:y doc;
            end;
            p
          end (y0, None) vlines
        in
        if !draw_marker then (PDF.line ~x1:(x0 +. x -. aw) ~y1:(last +. aw) ~x2:(x0 +. x +. aw) ~y2:(last -. aw) doc);
      in

      (* print_page *)
      let print_page key font =
        PDF.add_page doc;
        let hline x1 x2 y = PDF.line ~x1 ~y1:y ~x2 ~y2:y doc in

        let text = "{\192g}" in
        let family = Font.family key in
        let font_style = Font.style key in

        let font_size = font_size_pt /. scale in

        let line_gap = font_size *. Font.line_gap font in
        hline x0 (x0 +. x') (y0 +. line_gap);

        let baseline = font_size in
        hline x0 (x0 +. width_avail) (y0 +. baseline);

        let descent = font_size *. Font.descent font in
        hline x0 (x0 +. width_avail) (y0 +. baseline +. descent);

        (* Font preview *)
        PDF.cell
          ~font_family:family
          ~font_size:font_size_pt
          ~font_style
          ~border:[]
          ~text
          doc;

        (* Draw margins *)
        PDFGraphicsState.push doc;
        PDF.set_draw_color ~red:255 ~green:0 ~blue:0 doc;
        PDF.set_line_dash [1; 2] doc;
        PDF.rect ~x:x0 ~y:y0 ~width:width_avail ~height:height_avail ~style:`Outline doc;
        PDFGraphicsState.pop doc;

        (* Measurement lines *)
        print_vmesurements x' [
          (y0 +. line_gap /. 2. -. gap), None;
          (y0 +. line_gap /. 2. +. gap), Some "line gap";
          (y0 +. line_gap), None;
          (y0 +. baseline), None;
          (y0 +. baseline +. descent /. 2. -. gap), None;
          (y0 +. baseline +. descent /. 2. +. gap), Some "descent";
          (y0 +. baseline +. descent), None
        ];
        print_vmesurements x'' [
          (y0 +. baseline /. 2. -. gap), None;
          (y0 +. baseline /. 2. +. gap), Some "font size";
          (y0 +. baseline), None;
        ];

        (* Print label "baseline" *)
        let text = "baseline" in
        let tw = PDF.get_text_width label_font label_font_size text /. scale in
        PDF.set ~x:(x0 +. width_avail -. tw -. padding) ~y:(y0 +. baseline -. label_font_size /. scale -. padding /. 3.) doc;
        PDF.cell ~text ~font_family:label_font_family ~font_style:[`Italic] ~font_size:label_font_size doc;

        (* Print font name *)
        let font_size = 20. in
        let text = font.fontName in
        let tw = PDF.get_text_width font font_size text /. scale in
        PDF.set ~x:(x0 +. width_avail -. tw -. padding) ~y:(y0 +. baseline +. descent) doc;
        PDF.cell
          ~font_family:family
          ~font_size
          ~font_style
          ~border:[]
          ~text
          doc;
      in
      let ordered_fonts = ref [] in
      Hashtbl.iter (fun key font -> ordered_fonts := (key, font) :: !ordered_fonts) Font.fonts;
      let ordered_fonts = List.sort compare !ordered_fonts in
      List.iter (fun (k, f) -> print_page k f) ordered_fonts;
      (**  *)
      PDF.close_document doc;
      close_file();
    with ex -> begin
      close_file();
      raise ex
    end
  end;
  Printf.printf "
  +-----------------------------------------------------------------------------
  |
  | Please see output file %s
  |
  +-----------------------------------------------------------------------------

%!" filename;
  if Sys.os_type = "Win32" then ignore (Sys.command filename)
  else ignore (kprintf Sys.command "xpdf %s" filename)
end

let _ = PDFError.handle_error main ()
