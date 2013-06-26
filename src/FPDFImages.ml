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
open FPDFError

type info = {
  mutable image_name         : string;
  mutable image_index        : int;
  mutable image_obj          : int;
  mutable image_width        : int;
  mutable image_height       : int;
  mutable image_colorspace   : string;
  mutable image_bits         : int;
  mutable image_f            : string option;
  mutable image_palette      : string;
  mutable image_params       : string option;
  mutable image_trns         : int list option;
  mutable image_data         : string;
  mutable image_smask        : string option;
}


module Table = struct

  type t = {
    queue : info Queue.t;
    hash  : (string, info) Hashtbl.t;
  }

  let create () = { queue = Queue.create (); hash = Hashtbl.create 7 }

  let add name x (t : t) =
    x.image_name <- name;
    Queue.add x t.queue;
    Hashtbl.add t.hash name x;
    x.image_index <- Queue.length t.queue

  let length t = Queue.length t.queue

  let iter f t = Queue.iter f t.queue

  let find name t =
    try Some (Hashtbl.find t.hash name)
    with Not_found -> None


end

(** Jpeg *)
module Jpeg = struct

  exception Found of (int * int)

  (** get_dimensions *)
  let get_dimensions =
    let ff = Char.chr 0xFF in
    let soi_prefix = ['\xFF'; '\xD8'; '\xFF'] in
    fun data ->
      let data_size = String.length data in
      let i = ref 0 in
      (* Check for valid JPEG image *)
      let header = [data.[!i]; data.[!i + 1]; data.[!i + 2]] in
      if header = soi_prefix then begin
        match int_of_char data.[!i + 3] with
          | 0xE0 | 0xE1 ->
            (* Check for valid JPEG header (null terminated JFIF) *)
            i := !i + 4;
            let header = String.sub data 6 5 in
            if header = "JFIF\000" || header = "Exif\000" then begin
              (* Retrieve the block length of the first block since the first block will not contain the size of file *)
              let block_length = ref ((int_of_char data.[!i]) * 256 + (int_of_char data.[!i + 1])) in
              try
                while !i < data_size do
                  i := !i + !block_length; (* Increase the file index to get to the next block *)
                  if !i >= data_size then raise Exit else begin (* Check to protect against segmentation faults *)
                    if data.[!i] = ff then begin
                      (* Check that we are truly at the start of another block *)
                      match int_of_char data.[!i + 1] with
                        (* 0xFFC0 is the "Start of frame" marker which contains the file size *)
                        | 0xC0 | 0xC1 | 0xC2 | 0xC3 | 0xC5 | 0xC6 | 0xC7 | 0xC9 | 0xCA | 0xCB | 0xCD | 0xCE | 0xCF ->
                          (* The structure of the 0xFFC0 block is quite simple [0xFFC0][ushort length][uchar precision][ushort x][ushort y] *)
                          let height = (int_of_char data.[!i + 5]) * 256 + (int_of_char data.[!i + 6]) in
                          let width = (int_of_char data.[!i + 7]) * 256 + (int_of_char data.[!i + 8]) in
                          raise (Found (width, height));
                        | _ ->
                          i := !i + 2; (* Skip the block marker *)
                          block_length := (int_of_char data.[!i]) * 256 + (int_of_char data.[!i+1]);
                    end else (raise (Error ((Bad_image_format "JPEG"), "Invalid block")))
                  end
                done;
                raise (Error ((Bad_image_format "JPEG"), ""))
              with Found dim -> dim
            end else raise (Error ((Bad_image_format "JPEG"), "Not a valid JFIF string"))
          | _ -> raise (Error ((Bad_image_format "JPEG"), "Invalid JPEG header"))
      end else raise (Error ((Bad_image_format "JPEG"), "Not a valid SOI header"))

  (** parse *)
  let parse data =
    let width, height = get_dimensions data in
    let colsp, bits = "DeviceRGB", 8 in
    let result       = {
      image_name       = "";
      image_index      = -1;
      image_obj        = -1;
      image_width      = width;
      image_height     = height;
      image_colorspace = colsp;
      image_bits       = bits;
      image_f          = Some "DCTDecode";
      image_palette    = "";
      image_params     = None;
      image_trns       = None;
      image_data       = data;
      image_smask      = None;
    } in
    result
end

(** Png *)
module Png = struct

  let signature = "\x89\x50\x4E\x47\x0D\x0A\x1A\x0A"
  let signature_length = String.length signature

  (** read_int *)
  let read_int data i =
    let r0 = Char.code data.[!i] in
    let r1 = Char.code data.[!i+1] in
    let r2 = Char.code data.[!i+2] in
    let r3 = Char.code data.[!i+3] in
    let r = r0 lsl 8 in
    let r = r lor r1 in
    let r = r lsl 8 in
    let r = r lor r2 in
    let r = r lsl 8 in
    let r = r lor r3 in
    i := !i + 4;
    r

  (** get_size *)
  let read_IHDR data =
    try
      if String.sub data 0 signature_length = signature then
        let i                  = ref 16 in
        let width              = read_int data i in
        let i                  = ref 20 in
        let height             = read_int data i in
        let bit_depth          = int_of_char data.[24] in
        let color_type         = int_of_char data.[25] in
        let compression_method = int_of_char data.[26] in
        let filter_method      = int_of_char data.[27] in
        let interlace_method   = int_of_char data.[28] in
        width, height, bit_depth, color_type, compression_method, filter_method, interlace_method
      else raise (Error ((Bad_image_format "PNG"), "Invalid IHDR"))
    with Invalid_argument _ -> raise (Error ((Bad_image_format "PNG"), "Invalid IHDR"))

  (** iter_chunks *)
  let iter_chunks =
    fun f data ->
      try
        if String.sub data 0 signature_length = signature then
          let read_int = read_int data in
          let data_length = String.length data in
          let i = ref 8 in
          while !i < data_length do
            let chunk_size = read_int i in
            let chunk_type = String.sub data !i 4 in
            i := !i + 4;
            let chunk_start = !i in
            i := !i + chunk_size;
            let crc = read_int i in
            f chunk_type chunk_start chunk_size crc
          done
      with Invalid_argument _ -> raise (Error ((Bad_image_format "PNG"), ""))

  (** parse *)
  let parse data =
    match read_IHDR data with
      | width, height, bit_depth, color_type, compression_method, filter_method, interlace_method ->
        if bit_depth > 8 then raise (Error (Unsupported_16_bit_depth_image, ""));
        let color_space =
          if color_type = 0 || color_type = 4 then "DeviceGray"
          else if color_type = 2 || color_type = 6 then "DeviceRGB"
          else if color_type = 3 then "Indexed"
          else raise (Error (Unknown_color_type, ""))
        in
        let data_predictor = sprintf "/Predictor 15 /Colors %d /BitsPerComponent %d /Columns %d" (if color_space = "DeviceRGB" then 3 else 1) bit_depth width in
        let info = {
          image_name       = "";
          image_index      = -1;
          image_obj        = -1;
          image_width      = width;
          image_height     = height;
          image_colorspace = color_space;
          image_bits       = bit_depth;
          image_f          = Some "FlateDecode";
          image_palette    = "";
          image_params     = Some data_predictor;
          image_trns       = None;
          image_data       = "";
          image_smask      = None;
        } in
        iter_chunks begin fun chunk_type i chunk_size crc ->
          (*Printf.printf "%s: %d-%d\n%!" chunk_type i chunk_size;*)
          match chunk_type with
            | "PLTE" ->
              info.image_palette <- String.sub data i chunk_size;
            | "tRNS" ->
              let trns = String.sub data i chunk_size in
              info.image_trns <- (
                if color_type = 0 then Some [Char.code trns.[1]]
                else if color_type = 2 then Some [Char.code trns.[1]; Char.code trns.[3]; Char.code trns.[5]]
                else (try Some [String.index trns '\x00'] with Not_found -> None))
            | "IDAT" ->
              info.image_data <- info.image_data ^ (String.sub data i chunk_size)
            | "IEND" -> ()
            | _ -> ()
        end data;
        if color_space = "Indexed" && info.image_palette = "" then (raise (Error (Missing_palette, "")));
        if color_type >= 4 then begin
          info.image_data <- FPDFUtil.gz_uncompress info.image_data;
          let len = (if color_type = 4 then 2 else 4) * info.image_width in
          let color = Buffer.create 100 in
          let alpha = Buffer.create 100 in
          let color_length = if color_type = 4 then 1 else 3 in
          for i = 0 to info.image_height - 1 do
            let pos = (1 + len) * i in
            Buffer.add_char color info.image_data.[pos];
            Buffer.add_char alpha info.image_data.[pos];
            let line = String.sub info.image_data (pos + 1) len in
            let j = ref 0 in
            while !j < String.length line do
              Buffer.add_string color (String.sub line !j color_length);
              j := !j + color_length;
              Buffer.add_char alpha line.[!j];
              incr j;
            done;
          done;
          info.image_data <- FPDFUtil.gz_compress (Buffer.contents color);
          info.image_smask <- Some (FPDFUtil.gz_compress (Buffer.contents alpha));
        end;
        info
end

(** parse *)
let parse data =
  try
    ignore (Png.read_IHDR data);
    Png.parse data
  with Error ((Bad_image_format "PNG"), _) ->
    begin
      try
        ignore (Jpeg.get_dimensions data);
        Jpeg.parse data
      with Error ((Bad_image_format "JPEG"), _) ->
        raise (Error (Unsupported_image_format, ""))
    end;;

(** get_dimensions *)
let get_dimensions data =
  try
    let w, h, _, _, _, _, _ = Png.read_IHDR data in w, h
  with Error ((Bad_image_format "PNG"), _) ->
    begin
      try Jpeg.get_dimensions data
      with Error ((Bad_image_format "JPEG"), _) ->
        raise (Error (Unsupported_image_format, ""))
    end;





