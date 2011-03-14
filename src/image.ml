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

open PDFUtil

type t = { 
  image_name : string; 
  mutable image_obj : int;
  image_width : int;
  image_height : int; 
  image_colorspace : string;
  image_bits : int; 
  image_f : string option;
  image_palette : string;
  image_params : string option;
  image_trns : string list option;
  mutable image_data : string; 
}

let test = 0

let parse_jpg ~name ~width ~height data =
(*  let file = open_in_bin filename in
  let size = in_channel_length file in
  if size = 0 then failwith ("Missing or incorrect image file: " ^ filename);*)
(*  let img = Images.load filename [] in
  let (width, height) = Images.size img in
  let (colsp, bits, scan) = match img with
    | Images.Index8 x  -> "DeviceRGB", 8, Index8.get_scanline x
    | Images.Index16 x -> "DeviceRGB", 8, Rgb24.get_scanline (Index16.to_rgb24 x)
    | Images.Rgb24 x -> "DeviceRGB", 8, Rgb24.get_scanline x
    | Images.Rgba32 x -> "DeviceRGB", 8, Rgba32.get_scanline x
    | Images.Cmyk32 x  -> "DeviceCMYK", 8, Cmyk32.get_scanline x
  in*)
  let colsp, bits = "DeviceRGB", 8 in
(*  let data = fread filename in*)
  let result = {
    image_name = "";
    image_obj = -1;
    image_width = width;
    image_height = height;
    image_colorspace = colsp; 
    image_bits = bits;
    image_f = Some "DCTDecode";
    image_palette = "";
    image_params = None; 
    image_trns = None;
    image_data = data
  } in
(*  close_in file;*)
  result 

let parse ?format ~width ~height name data =
  let format = match format with Some fmt -> fmt
    | None ->
      try
        let pos = String.rindex name '.' in
        String.sub name (pos + 1) ((String.length name) - pos - 1)
      with Not_found ->
        failwith ("Image file has no extension and no type was specified: \"" ^ name ^ "\"") in
  let format = String.lowercase format in
  match format with
    | "jpg" | "jpeg" -> parse_jpg ~name ~width ~height data
    | "png" -> {
      image_name = "";
      image_obj = -1;
      image_width = -1;
      image_height = -1;
      image_colorspace = "";
      image_bits = 8;
      image_f = Some "DCTDecode";
      image_palette = "";
      image_params = None;
      image_trns = None;
      image_data = "";
    } (*parse_png filename*)
    | _ -> failwith ("Unsupported image format \"" ^ format ^ "\".")







