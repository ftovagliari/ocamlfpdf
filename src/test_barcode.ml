open Printf

let main () = begin
  set_binary_mode_out stdout true;
  (* *)
  let doc = PDF.create ~orientation:PDF.Portrait () in
  let margin = 25. in
  let space = 2. in
  let barcode_text_size = 8.0 in
  doc#set_margins ~left:margin ~top:margin ();
  doc#add_page ();
  doc#set_font ~family:`Courier ~style:[] ~size:12.0 ();
  (*  *)
  let tests = [|
    "0014180300030G2"; "123"; "ABC"
  |] in
  let height_barcode = 8. in
  let height_text = 3.5 in



  (** Code 39 *)
  doc#set_font ~size:20. ();
  doc#multi_cell ~width:100. ~height:5. ~align:`Left ~border:[`All] ~text:"Code 39" ();
  let y = ref (doc#y +. space) in
  Array.iteri begin fun i baseline ->
    Array.iteri begin fun j barcode ->
      Barcode.Code39.write ~x:margin ~y:!y ~baseline ~height:height_barcode ~barcode ~text:barcode_text_size doc;
      y := !y +. height_barcode +. height_text +. 5.;
    end tests;
    y := !y +. space;
    doc#line ~x1:margin ~y1:!y ~x2:doc#page_width ~y2:!y;
    y := !y +. space;
    doc#newline ();
  end [|0.5; 0.7; 0.9; 1.0|];



  (** Code 128 *)
  let tests = [|
    "0014180300030020"; "1230"; "00"
  |] in
  doc#add_page();
  doc#set_font ~size:20. ();
  doc#multi_cell ~width:100. ~height:5. ~align:`Left ~border:[`All] ~text:"Code 128" ();
  let y = ref (doc#y +. space) in
  Array.iteri begin fun i baseline ->
    Array.iteri begin fun j barcode ->
      Barcode.Code128C.write ~x:margin ~y:!y ~height:height_barcode ~barcode ~text:barcode_text_size doc;
      y := !y +. height_barcode +. height_text;
    end tests;
    y := !y +. space;
    doc#line ~x1:margin ~y1:!y ~x2:doc#page_width ~y2:!y;
    y := !y +. space;
    doc#newline ();
  end [|0.5; 1.0; 1.5; 2.0|];
(*  (** Code EAN 13 *)
  let tests = [|
    "123"; "123"; "456"
  |] in
  doc#add_page();
  doc#set_font ~size:20. ();
  doc#multi_cell ~width:100. ~height:5. ~align:`Left ~border:[`All] ~text:"EAN 13" ();
  let y = ref (doc#y +. space) in
  Array.iteri begin fun i baseline ->
    Array.iteri begin fun j barcode ->
      Barcode.EAN13.write ~x:margin ~y:!y ~height:height_barcode ~barcode doc;
      y := !y +. height_barcode +. height_text;
    end tests;
    y := !y +. space;
    doc#line ~x1:margin ~y1:!y ~x2:doc#page_width ~y2:!y;
    y := !y +. space;
    doc#newline ();
  end [|0.5; 1.0; 1.5; 2.0|];*)
  (** -------  *)
  let filename = "test_barcode.pdf" in
  doc#print_document ~filename;
  Sys.command filename;
  exit 0
end



let _ = main ()


