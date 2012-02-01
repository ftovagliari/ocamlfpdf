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

(** Compression *)
(*let gz_compress txt =
  try Cryptokit.transform_string (Cryptokit.Zlib.compress ()) txt
  with Cryptokit.Error Cryptokit.Compression_not_supported -> txt
let gz_uncompress txt =
  try Cryptokit.transform_string (Cryptokit.Zlib.uncompress ()) txt
  with Cryptokit.Error Cryptokit.Compression_not_supported -> txt*)
(*let gz_compress txt = Cryptokit.transform_string (Cryptokit.Zlib.compress ()) txt
let gz_uncompress txt = Cryptokit.transform_string (Cryptokit.Zlib.uncompress ()) txt*)
let gz_compress txt = txt
let gz_uncompress txt = txt


(** excape *)
let escape =
  let ro, rc, rbs = Str.regexp "(", Str.regexp ")", Str.regexp "\\" in fun s ->
    Str.global_replace rc "\\)"
      (Str.global_replace ro "\\("
        (Str.global_replace rbs "\\\\" s))

(** pdf_string *)
let pdf_string s = "(" ^ (escape s) ^ ")"

(** call_and_restore *)
let call_and_restore ~pre f x ~post =
  let old = pre() in
  let result = f x in
  post old;
  result

(** utf8_to_utf16 *)
let utf8_to_utf16 text =
  (* Convert UTF-8 to UTF-16BE with BOM *)
  let nb = String.length text in
  let res = Buffer.create nb in
  Buffer.add_string res "\xFE\xFF";
  let i = ref (-1) in
  while !i < nb - 1 do
    incr i;
    let c1 = Char.code (String.get text !i) in
    if c1 >= 224 then begin
      (* 3-byte character *)
      incr i;
      let c2 = Char.code (String.get text !i) in
      incr i;
      let c3 = Char.code (String.get text !i) in
      Buffer.add_char res (Char.chr (((c1 land 0x0F) lsl 4) + ((c2 land 0x3C) lsr 2)));
      Buffer.add_char res (Char.chr (((c2 land 0x03) lsl 6) + (c3 land 0x3F)));
    end else if c1 >= 192 then begin
      (* 2-byte character *)
      incr i;
      let c2 = Char.code (String.get text !i) in
      Buffer.add_char res (Char.chr ((c1 land 0x1C) lsr 2));
      Buffer.add_char res (Char.chr (((c1 land 0x03) lsl 6) + (c2 land 0x3F)));
    end else begin
      (* Single-byte character *)
      Buffer.add_string res "\\0";
      Buffer.add_char res (Char.chr c1);
    end
  done;
  Buffer.contents res

(** memo *)
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

let rec group_by f ll =
  List.map (fun (a, r) -> (a, !r)) (List.fold_left begin fun groups a ->
    let g = f a in
    try
      let group = List.assoc g groups in
      group := a :: !group;
      groups
    with Not_found -> ((g, ref [a]) :: groups)
  end [] ll);;

























