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

open Printf

type t = {
  doc                  : PDF.document;
  mutable root_obj     : int;
  mutable tree         : node list;
  mutable length       : int;
}

and node = {
  id       : int;
  text     : string;
  page     : int;
  y        : float;
  mutable obj      : int;
  mutable children : node list;
}

type index = int

(** tree_iter *)
let rec tree_iter f lev nodes =
  let nodes = List.rev nodes in
  let i = ref 0 in
  let len = List.length nodes - 1 in
  List.iter begin fun node ->
    f (!i, len) lev node;
    tree_iter f (lev + 1) node.children;
    incr i;
  end nodes
;;

(** tree_length *)
let rec tree_length nodes =
  List.fold_left (fun sum child -> sum + tree_length child.children) (List.length nodes) nodes
;;

(** tree_append *)
let tree_append child parent =
  let rec f = function
    | [] -> false
    | nodes ->
      begin
        try
          let parent = List.find (fun d -> d.id = parent) nodes in
          parent.children <- child :: parent.children;
          true
        with Not_found -> begin
          List.exists (fun x -> x) (List.map (fun node -> f node.children) nodes);
        end
      end
  in
  function nodes -> (if f nodes then nodes else (child :: nodes));
;;

(** instances *)
let instances = ref []

(** create *)
let create doc =
  let bookmark = {
    doc          = doc;
    root_obj     = 0;
    tree         = [];
    length       = 0;
  } in
  PDF.add_resource begin fun () ->
    if List.length bookmark.tree > 0 then begin
      bookmark.root_obj <- 1 + bookmark.length + PDF.current_object_number doc;
      (* Print Resources *)
      let prev_sibling = ref [] in
      tree_iter begin fun (i, last) level node ->
        PDF.new_obj doc;
        node.obj <- PDF.current_object_number doc;
        PDF.print doc "<</Title %s " (PDFUtil.pdf_string node.text);
        let parent =
          try (List.assoc (level - 1) !prev_sibling).obj
          with Not_found -> bookmark.root_obj
        in
        PDF.print doc "/Parent %d 0 R " parent;
        if node.children <> [] then begin
          PDF.print doc "/First %d 0 R " (PDF.current_object_number doc + 1);
          PDF.print doc "/Last %d 0 R " (PDF.current_object_number doc + List.length node.children);
        end;
        if i > 0 && i <= last && !prev_sibling <> [] then begin
          let prev_sibling_length =
            try tree_length (List.assoc level !prev_sibling).children
            with Not_found -> assert false
          in
          PDF.print doc "/Prev %d 0 R " (PDF.current_object_number doc - prev_sibling_length - 1);
        end;
        if i >= 0 && i < last then begin
          PDF.print doc "/Next %d 0 R " (PDF.current_object_number doc + tree_length node.children + 1);
        end;
        PDF.print doc "/Dest [%d 0 R /XYZ 0 %.2f null] " (3 + 2 * node.page) node.y;
        PDF.print doc "/Count 0>>";
        PDF.print doc "endobj\n";
        prev_sibling := (level, node) :: !prev_sibling;
      end 0 bookmark.tree;
      (* Outline root *)
      PDF.new_obj doc;
      let first =
        match bookmark.tree with
          | [] -> assert false
          | items -> (List.hd (List.rev items)).obj
      in
      (* The first toplevel item *)
      PDF.print doc "<</Type /Outlines /First %d 0 R " first;
      let last =
        match bookmark.tree with
          | [] -> assert false
          | items -> (List.hd items).obj
      in
      (* The last toplevel item *)
      PDF.print doc "/Last %d 0 R>>endobj\n" last;
    end
  end doc;
  (* Add catalog *)
  PDF.add_catalog begin fun () ->
    if List.length bookmark.tree > 0 then begin
      PDF.print doc "/Outlines %d 0 R " bookmark.root_obj;
      PDF.print doc "/PageMode /UseOutlines "
    end;
    instances := List.filter (fun (k, _) -> doc != k) !instances
  end doc;
  bookmark
;;

(** add *)
let add ?(text="") ?page ?(y=0.0) ?(parent=0) doc =
  let bookmark =
    try List.assq doc !instances
    with Not_found ->
      let bm = create doc in
      instances := (doc, bm) :: !instances;
      bm
  in
  let y = match y with -1. -> PDF.y doc | y -> y in
  let id = bookmark.length + 1 in
  let child = {
    id       = id;
    text     = text;
    y        = (PDF.page_height doc -. y) *. PDF.scale doc;
    page     = (match page with None -> PDF.page_no doc | Some p -> p);
    obj      = 0;
    children = [];
  } in
  bookmark.tree <- tree_append child parent bookmark.tree;
  bookmark.length <- bookmark.length + 1;
  id
;;


