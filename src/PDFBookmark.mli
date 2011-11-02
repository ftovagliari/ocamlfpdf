(*

  OCaml-FPDF
  Copyright (C) 2011 Francesco Tovagliari

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

(** Bookmarks *)

type index

(**
  @param text The bookmark title. The title must be encoded in ISO-8859-1. Use {!PDFUtil.utf8_to_utf16} for UTF-8.
  @param parent The index of the parent item.
  @param page The page number of the bookmark. Default value: current page.
  @param y The {i y} position of the bookmark destination in the page. [-1] means the current position. Default value: [0].
  @return An index representing the bookmark, for use in subsequent calls as parameter "parent" to append child elements.
*)
val add : ?text:string -> ?page:int -> ?y:float -> ?parent:index -> PDF.document -> index

