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

let _ =
  let len = 1000 in
  let text = Buffer.create len in
  for _i = 1 to len do
    Buffer.add_char text (Char.chr (32 + Random.int 95))
  done;
  let c = FPDFUtil.gz_compress (Buffer.contents text) in
  let d = FPDFUtil.gz_uncompress c in
  Printf.printf "%S\n%S\n%.2f\n%!" c d (float (String.length c) /. float (String.length d))
;;
