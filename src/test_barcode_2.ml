let lpad str len ch =
  let len' = String.length str in
  if len' > len then String.sub str 0 len
  else
    let result = String.make len ch in
    String.blit str 0 result (len - len') len';
    result

let main () = begin
(*  let text =
    let file = open_in_bin "text.txt" in
    let len = in_channel_length file in
    let buf = Buffer.create len in
    Buffer.add_channel buf file len;
    close_in file;
    Buffer.contents buf
  in*)
  set_binary_mode_out stdout true;
  (* *)
  let doc = PDF.create ~orientation:PDF.Portrait () in
  doc#set_compression false;
  doc#set_margins ~left:10. ~top:10. ();
(*  PDF.set_heder begin fun () ->
    PDF.set_font ~family:`Times ~style:[`Bold; `Italic] ~size:9.0 doc;
    PDF.cell ~border:[`ALL] ~width:100. ~height:10. ~text:"Header" doc;
    PDF.newline ~height:10. doc;
  end doc;
  PDF.set_footer begin fun () ->
    PDF.set_font ~family:`Times ~style:[`Italic] ~size:9.0 doc;
    PDF.cell ~width:10. ~height:10. ~text:"Footer" doc;
    PDF.newline ~height:5. doc;
  end doc;*)
  doc#add_page ();
(*  PDF.set_font ~family:`Courier ~style:[`Italic] ~size:12.0 doc;
  PDF.cell ~width:190. ~height:100. ~border:[`ALL] doc;
  PDF.newline doc;
  PDF.multi_cell ~width:100. ~height:5. ~align:`JUSTIFIED ~border:[`ALL] ~text doc;
  let text = String.create 256 in
  (*  *)
  for i = 0 to 255 do text.[i] <- Char.chr(i) done;
  PDF.set_font ~family:`Times ~style:[`Italic] ~size:12.0 doc;
  PDF.multi_cell ~width:100. ~height:5. ~align:`LEFT ~border:[`ALL] ~text doc;
  (*  *)
  for i = 0 to 255 do text.[i] <- Char.chr(i) done;
  PDF.set_font ~family:`Times ~style:[`Bold] ~size:12.0 doc;
  PDF.multi_cell ~width:100. ~height:5. ~align:`LEFT ~border:[`ALL] ~text doc;
  (*  *)
  for i = 0 to 255 do text.[i] <- Char.chr(i) done;
  PDF.set_font ~family:`Times ~style:[`Bold; `Italic] ~size:12.0 doc;
  PDF.multi_cell ~width:100. ~height:5. ~align:`LEFT ~border:[`ALL] ~text doc;
  (*  *)
  for i = 0 to 255 do text.[i] <- Char.chr(i) done;
  PDF.set_font ~family:`Courier ~style:[`Italic] ~size:12.0 doc;
  PDF.multi_cell ~width:100. ~height:5. ~align:`LEFT ~border:[`ALL] ~text doc;
  (*  *)
  for i = 0 to 255 do text.[i] <- Char.chr(i) done;
  PDF.set_font ~family:`Courier ~style:[`Bold] ~size:12.0 doc;
  PDF.multi_cell ~width:100. ~height:5. ~align:`LEFT ~border:[`ALL] ~text doc;
  (*  *)
  for i = 0 to 255 do text.[i] <- Char.chr(i) done;
  PDF.set_font ~family:`Courier ~style:[`Bold; `Italic; `Underline] ~size:10.0 doc;
  PDF.multi_cell ~width:100. ~height:5. ~align:`LEFT ~border:[`ALL] ~text doc;
  (*  *)
  PDF.add_image ~file:"jpg.jpg" ~image_width:331 ~image_height:500 ~width:20 ~height:30 ~x:100. ~y:100. doc;*)


(*




*)
  let x, y = 10., 0. in
  let height = 10. in
  let space = 10. in
  doc#multi_cell ~width:100. ~height:5. ~align:`Left ~border:[`All] ~text:"EAN13" ();
  let barcode = "0123456789012" in
  Barcode.EAN13.write ~x ~y ~height ~barcode doc;
  let y = height +. space in
  Barcode.Code39.write ~x ~y ~height ~barcode doc;
  let y = y +. height +. 11. /. doc#scale in
  doc#set_font ~size:12. ();
  doc#text ~x ~y ~text:barcode ();

  let y = y +. height +. space in
  let barcode = "9012" in
  Barcode.Code128.write ~x ~y (*~width:1.*) ~height ~barcode ~text:12. doc;
  let y = y +. height +. space in
  let barcode = "801011012510" in
  Barcode.Code128.write ~x ~y (*~width:1.*) ~height ~barcode ~text:12. doc;
  let y = y +. height +. space in
  let barcode = "08003670645917" in
  Barcode.Code128.write ~x ~y (*~width:1.*) ~height ~barcode ~text:12. doc;

(*  let y = y +. height +. space in
  let barcode = "0014180200030G2" in
  Barcode.Code128.write ~x ~y (*~width:1.*) ~height ~barcode ~text:12. doc;
  let y = y +. height +. space in
  let barcode = "0014180300030G2" in
  Barcode.Code128.write ~x ~y (*~width:1.*) ~height ~barcode ~text:12. doc;*)

  let y = y +. height +. space in
  let barcode = "00123456789012" in
  Barcode.Code39.write ~x ~y (*~width:1.*) ~text:10. ~height ~barcode doc;
  let y = y +. height +. space in
  let barcode = "801011012510" in
  Barcode.Code39.write ~x ~y (*~width:1.*) ~text:10. ~height ~barcode doc;
  let y = y +. height +. space in
  let barcode = "08003670645917" in
  Barcode.Code39.write ~x ~y (*~width:1.*) ~text:10. ~height ~barcode doc;
  let y = y +. height +. space in
  let barcode = "0014180300030G2" in
  Barcode.Code39.write ~x ~y (*~width:1.*) ~text:10. ~height ~barcode doc;

  let filename = "test_barcode_2.pdf" in
  doc#print_document ~filename;
  Sys.command filename;
  exit 0
end



let _ = Printexc.print main ()


