open Fpdf_images.Jpeg
let (//) = Filename.concat

let _ =
  let dirname = Filename.dirname Sys.executable_name in
  let filename = "Lena.jpg" in
  let filename = "as.jpg" in
  let filename = "22008595.jpg" in
  let filename = dirname // filename in
  Printf.printf " %s %b\n%!" filename (Sys.file_exists filename);
  let data = Fpdf_util.fread filename in
  let info = Fpdf_images.Jpeg.get_info data in
  Printf.printf "%s: %d x %d -- %d, %d\n%!" filename
    info.jpeg_width info.jpeg_height
    info.jpeg_P info.jpeg_Nf;