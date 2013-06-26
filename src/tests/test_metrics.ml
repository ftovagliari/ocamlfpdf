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
open FPDFDocument

let (//) = Filename.concat


let sample = "\
Lorem ipsum dolor sit amet, consectetur <SPAN underline='single'>adipiscing</SPAN> elit. Vestibulum imperdiet \
diam id diam aliquam porta at ac urna. <SPAN underline='low'>Quisque justo</SPAN> est, cursus ac rutrum sed, \
accumsan sed arcu. Maecenas at urna velit."

let sample_long = "Fusce sagittis ullamcorper urna vel\
commodo. Integer gravida feugiat eros nec vestibulum. Praesent quis gravida mi. \
Suspendisse eu magna enim. Cras quis augue sed purus posuere consequat. \
Mauris vestibulum tempus ipsum ac iaculis. Nunc lorem augue, semper eget gravida\
sed, placerat volutpat nunc. Quisque sed tortor diam, ut blandit justo. \
Pellentesque consectetur mauris tellus. Integer pellentesque elit vel orci\
venenatis sollicitudin. Sed lacinia magna vitae ante elementum suscipit. "

let main () = begin
  let filename_tmp = Sys.argv.(0) ^ ".tmp" in
  let filename = Sys.argv.(0) ^ ".pdf" in
  let outchan = open_out_bin filename_tmp in
  let close_file () = close_out outchan in
  begin
    try
      let doc               = FPDF.create ~orientation:`Landscape ~outchan () in
      let margin            = 10.0 in
      let margin_top        = margin in
      let margin_right      = margin in
      let margin_bottom     = margin in
      let margin_left       = margin in
      let width_avail       = FPDFPage.page_width doc -. margin_left -. margin_right in
      let height_avail      = FPDFPage.page_height doc -. margin_top -. margin_bottom in
      let scale             = FPDF.scale doc in
      let x0                = margin_left in
      let y0                = margin_top in
      let padding           = 3. in
      let label_font_size   = 8. in
      let label_font_family = `Helvetica in
      let label_font        = Font.find Font.Helvetica_Oblique in
      let font_size_pt      = (height_avail *. 0.23) *. scale in
      let x'                = width_avail *. 0.75 in
      let x''               = width_avail *. 0.85 in
      let gap               = label_font_size /. scale in
      let sample_text =     "{\192g}" in
      let sample_text =     "{\192g\131\190}" in

      FPDF.set_margins ~top:margin_top ~right:margin_right ~bottom:margin_bottom ~left:margin_left doc;
      FPDF.set_line_cap `Round doc;
      FPDF.set_line_width 0.25 doc;

      FPDFFont.embed_font ~family:`CenturySchoolbook ~style:[] doc;
      FPDFFont.embed_font ~family:`CenturySchoolbook ~style:[`Italic] doc;
      FPDFFont.embed_font ~family:`CenturySchoolbook ~style:[`Bold] doc;
      FPDFFont.embed_font ~family:`CenturySchoolbook ~style:[`Bold; `Italic] doc;
      FPDFFont.embed_font ~family:`CMUSerif ~style:[] doc;
      FPDFFont.embed_font ~family:`CMUSerif ~style:[`Bold] doc;
      FPDFFont.embed_font ~family:`CMUSerif ~style:[`Italic] doc;
      FPDFFont.embed_font ~family:`CMUSerif ~style:[`Bold; `Italic] doc;
      FPDFFont.embed_font ~family:`CMUSerif_BoldNonextended ~style:[] doc;
      FPDFFont.embed_font ~family:`CMUSansSerif ~style:[] doc;
      FPDFFont.embed_font ~family:`CMUSansSerif ~style:[`Bold] doc;
      FPDFFont.embed_font ~family:`CMUSansSerif ~style:[`Italic] doc;
      FPDFFont.embed_font ~family:`CMUSansSerif ~style:[`Bold; `Italic] doc;
      FPDFFont.embed_font ~family:`CMUSansSerif_DemiCondensed ~style:[] doc;

      (* print_vmesurements *)
      let print_vmesurements x vlines =
        let aw = 1. in
        let draw_marker = ref true in
        let last, _ =
          List.fold_left begin fun (prev, _) ((y, label) as p) ->
            begin
              match label with
                | Some text ->
                  let tw = FPDF.get_text_width label_font label_font_size text /. scale in
                  let x = x0 +. x -. tw /. 2. in
                  let y = y -. 3. /. 2. *. gap in
                  FPDF.set ~x ~y doc;
                  FPDF.cell ~text ~font_family:label_font_family ~font_style:[`Italic] ~font_size:label_font_size ~border:[] doc;
                  draw_marker := false;
                | None ->
                  if !draw_marker then (FPDF.line ~x1:(x0 +. x -. aw) ~y1:(prev +. aw) ~x2:(x0 +. x +. aw) ~y2:(prev -. aw) doc);
                  draw_marker := true;
                  FPDF.line ~x1:(x0 +. x) ~y1:prev ~x2:(x0 +. x) ~y2:y doc;
            end;
            p
          end (y0, None) vlines
        in
        if !draw_marker then (FPDF.line ~x1:(x0 +. x -. aw) ~y1:(last +. aw) ~x2:(x0 +. x +. aw) ~y2:(last -. aw) doc);
      in

      (* Printable Characters *)
      let characters = ref [] in
      for i = 255 downto 32 do
        let c = Char.chr i in
        let s =
          match c with
            | '&' -> "&amp;"
            | '\'' -> "&apos;"
            | '"' -> "&quot;"
            | '<' -> "&lt;"
            | '>' -> "&gt;"
            (*| '\\' -> "\\" (*"\092"*)*)
            | _ -> String.make 1 c
        in
        characters := (sprintf "<SPAN family='Courier' style='' size='7'>%d=</SPAN>%s" i s) :: !characters
      done;
      let characters = String.concat " " !characters in

      (* print_page *)
      let print_page key font =
        FPDF.add_page doc;
        let hline x1 x2 y = FPDF.line ~x1 ~y1:y ~x2 ~y2:y doc in

        let font_size = font_size_pt /. scale in

        let line_gap = font_size *. float (Font.line_gap font) /. 1000. in
        hline x0 (x0 +. x') (y0 +. line_gap);

        let baseline = font_size *. float (Font.baseline font) /. 1000. in
        hline x0 (x0 +. width_avail) (y0 +. baseline);

        let descent = font_size *. float (Font.descent font) /. 1000. in
        hline x0 (x0 +. width_avail) (y0 +. baseline +. descent);

        (* Font preview *)
        FPDF.cell
          ~font_family:font.fontFamily
          ~font_size:font_size_pt
          ~font_style:(Font.style font)
          ~border:[]
          ~text:sample_text
          doc;

        (* Draw margins *)
        FPDFGraphicsState.push doc;
        FPDF.set_draw_color ~red:255 ~green:0 ~blue:0 doc;
        FPDF.set_line_dash [1; 2] doc;
        FPDF.rect ~x:x0 ~y:y0 ~width:width_avail ~height:height_avail ~style:`Outline doc;
        FPDFGraphicsState.pop doc;

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
          (y0 +. baseline /. 2. +. gap), Some "baseline";
          (y0 +. baseline), None;
        ];

        (* Print label "origin" *)
        let text = "origin" in
        let x0' = x0 +. FPDF.line_width doc in
        let y0' = y0 +. FPDF.line_width doc in
        let x = x0' +. 3. *. padding in
        let y = y0' +. 3. *. padding in
        let aw = 2. in
        FPDF.set ~x ~y doc;
        FPDF.cell ~text ~font_family:label_font_family ~font_style:[`Italic] ~font_size:label_font_size doc;
        FPDF.set_line_width 0.1 doc;
        FPDF.line ~x1:x0' ~y1:x0' ~x2:x ~y2:y doc;
        FPDF.line ~x1:x0' ~y1:x0' ~x2:(x0' +. aw /. 3.) ~y2:(y0' +. aw) doc;
        FPDF.line ~x1:x0' ~y1:x0' ~x2:(x0' +. aw) ~y2:(y0' +. aw /. 3.) doc;
        FPDF.set_line_width 0.25 doc;

        (*(* Print label "baseline" *)
          let text = "baseline" in
          let tw = FPDF.get_text_width label_font label_font_size text /. scale in
          FPDF.set ~x:(x0 +. width_avail -. tw -. padding) ~y:(y0 +. baseline -. label_font_size /. scale -. padding /. 3.) doc;
          FPDF.cell ~text ~font_family:label_font_family ~font_style:[`Italic] ~font_size:label_font_size doc;*)

        (* Print font name *)
        let font_size = 20. in
        let text = font.fontName in
        let tw = FPDF.get_text_width font font_size text /. scale in
        FPDF.set ~x:(x0 +. width_avail -. tw -. padding) ~y:(y0 +. baseline +. descent) doc;
        FPDF.cell
          ~font_family:font.fontFamily
          ~font_size
          ~font_style:(Font.style font)
          ~border:[]
          ~text
          doc;

        (*  *)
        let y = y0 +. baseline +. descent +. font_size *. (1. +. float (Font.descent font) /. 1000.) /. scale +. 2. *. padding in
        let padding = (2.,2.,2.,2.) in
        FPDF.set_font ~family:font.fontFamily ~size:11. ~style:(Font.style font) doc;
        let markup = FPDFMarkup.prepare ~width:width_avail ~padding ~markup:characters doc in
        markup.FPDFMarkup.print ~x:x0 ~y ();

        let y = y +. markup.FPDFMarkup.height in
        FPDF.set_font ~family:font.fontFamily ~size:18. ~style:(Font.style font) doc;
        let markup = FPDFMarkup.prepare ~width:width_avail ~padding ~markup:sample doc in
        markup.FPDFMarkup.print ~x:x0 ~y ();

        let y = y +. markup.FPDFMarkup.height in
        FPDF.set_font ~family:font.fontFamily ~size:11. ~style:(Font.style font) doc;
        let markup = FPDFMarkup.prepare ~width:width_avail ~padding ~markup:sample_long doc in
        markup.FPDFMarkup.print ~x:x0 ~y ()
      in
      let ordered_fonts = ref [] in
      Hashtbl.iter begin fun key font ->
        (*if List.mem key [CMUSerif_Bold; CMUSerif_BoldNonextended] then*)
        ordered_fonts := (key, font) :: !ordered_fonts
      end Font_loader.fonts;
      let ordered_fonts = List.sort compare !ordered_fonts in
      List.iter (fun (k, f) -> print_page k f) ordered_fonts;
      (**  *)
      FPDF.close_document doc;
      close_file();
      let cpdf = Pdfread.pdf_of_file None filename_tmp in
      if Sys.file_exists filename_tmp then Sys.remove filename_tmp;
      Pdfwrite.pdf_to_file_options true None false cpdf filename
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

let _ = FPDFError.handle_error main ()
