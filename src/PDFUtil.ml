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
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*)
    
open Cryptokit  
open Cryptokit.Cipher 

exception Continue 

(** Legge un file e ne mette il contenuto in un [Buffer]. *)
let fread filename =
  let inchan = open_in_bin filename in
  let length = in_channel_length inchan in   
  let buf = Buffer.create length in 
  begin
    try
      while true do
        Buffer.add_channel buf inchan length
      done
    with End_of_file -> ();
  end;
  close_in inchan; 
  buf
 
let re_cr = Str.regexp "\r" 

let trim =
   let replace = Str.global_replace (Str.regexp "\\(^[ \t\r\n]+\\)\\|\\([ \t\r\n]+$\\)") in
   fun str -> replace "" str

let rtrim =
   let replace = Str.global_replace (Str.regexp "[ \t\r\n]+$") in
   fun str -> replace "" str

let ltrim =
   let replace = Str.global_replace (Str.regexp "^[ \t\r\n]+") in
   fun str -> replace "" str

let may ~f x =
  match x with None -> () 
  | Some x -> let _ = f x in () 

let remove_dupl l =
  List.rev (List.fold_left (fun acc y -> if List.mem y acc then acc else y :: acc) [] l)

(** Compressione
  * sdsdkfskl
  * sdklls
  *
  *)
(*let gz_compress txt = try transform_string (Zlib.compress ()) txt with Error Compression_not_supported -> txt
let gz_uncompress txt = try transform_string (Zlib.uncompress ()) txt with Error Compression_not_supported -> txt*)

let gz_compress txt = transform_string (Zlib.compress ()) txt
let gz_uncompress txt = transform_string (Zlib.uncompress ()) txt

let escape =
  let ro, rc, rbs = Str.regexp "(", Str.regexp ")", Str.regexp "\\" in fun s ->
    Str.global_replace rc "\\)"
      (Str.global_replace ro "\\("  
        (Str.global_replace rbs "\\\\" s))

let pdf_string s = "(" ^ (escape s) ^ ")"
(** call_and_restore *)
let call_and_restore ~pre f x ~post = 
  let old = pre() in
  let result = f x in
  post old;
  result

let memo ~f =
  let table = Hashtbl.create 1 in
  fun ?(force=fun _ -> false) key ->
    try
      let data = Hashtbl.find table key in
      if force data then begin
        Hashtbl.remove table key;
        raise Not_found;
      end;
      data
    with Not_found ->
      let data = f key in
      Hashtbl.add table key data;
      data



























