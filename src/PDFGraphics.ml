(*

  OCaml-FPDF
  Copyright (C) 2010-2012 Francesco Tovagliari

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

open PDFTypes
open PDFDocument
open Image
open Printf

(** draw_color *)
let set_draw_color ~red ?(green=(-1)) ?(blue=(-1)) doc =
  if (red = 0 && green = 0 && blue = 0) || green = -1 then
    (doc.drawColor <- sprintf "%.3f G" ((float red) /. 255.))
  else begin doc.drawColor <- sprintf "%.3f %.3f %.3f RG"
    ((float red) /. 255.) ((float green) /. 255.) ((float blue) /. 255.)
  end;
  if doc.page >= 0 then print_buffer doc "%s\n" doc.drawColor;
  doc.draw_color_rgb <- (red, green, blue)

let draw_color doc = doc.draw_color_rgb

(** fill_color *)
let set_fill_color ~red ?(green=(-1)) ?(blue=(-1)) doc =
  if (red = 0 && green = 0 && blue = 0) || green = -1 then
    (doc.fillColor <- sprintf "%.3f g" ((float red) /. 255.))
  else begin doc.fillColor <- sprintf "%.3f %.3f %.3f rg"
    ((float red) /. 255.) ((float green) /. 255.) ((float blue) /. 255.)
  end;
  doc.colorFlag <- doc.fillColor <> doc.textColor;
  if doc.page >= 0 then print_buffer doc "%s\n" doc.fillColor;
  doc.fill_color_rgb <- (red, green, blue)

let fill_color doc = doc.fill_color_rgb

(** Images *)
let find_image name doc =
  let equals = fun {Image.image_name = name'} -> name' = name in
  let rec find i = function
    | a :: b -> if equals a then (i, a) else find (i + 1) b
    | [] -> raise Not_found
  in
  find 1 doc.images

let image ~name ~data ~x ~y ~image_width ~image_height ?(width=0.) ?(height=0.) ?format ?link doc =
  let index, info =
    try find_image name doc
    with Not_found -> begin
      let info = Image.parse ?format ~width:image_width ~height:image_height name data in
      doc.images <- info :: doc.images;
      (List.length doc.images), info
    end in
(*    let width, height = swidth, sheight in*)
    let width, height = match width, height with
      | 0., 0. -> ((float info.image_width) /. doc.k), ((float info.image_height) /. doc.k)
      | _ -> width, height in
    let width = if width = 0. then height *. (float info.image_width) /. (float info.image_height)
      else width in
    let height = if height = 0. then width *. (float info.image_height) /. (float info.image_width)
      else height in
    print_buffer doc "q %.2f 0 0 %.2f %.2f %.2f cm /I%d Do Q\n" (width *. doc.k) (height *. doc.k)
      (x *. doc.k) ((doc.h -. (y +. height)) *. doc.k) index;
    match link with
      | None -> ()
      | Some l -> add_link ~x ~y ~width ~height ~link:l ();;

(** line *)
let line ~x1 ~y1 ~x2 ~y2 doc =
  print_buffer doc "%.2f %.2f m %.2f %.2f l S\n"
    (x1 *. doc.k) ((doc.h -. y1) *. doc.k) (x2 *. doc.k) ((doc.h -. y2) *. doc.k)

(** rect *)
let rect ?x ?y ~width ~height ?radius ?(style=(`Outline : rect_style)) doc =
  let x = match x with None -> doc.pos_x | Some x -> x in
  let y = match y with None -> doc.pos_y | Some y -> y in
  let op = match style with
    | `Fill -> 'f'
    | `Both -> 'B'
    | `Outline -> 'S' in
  match radius with
    | None -> print_buffer doc "%.2f %.2f %.2f %.2f re %c\n"
      (x *. doc.k) ((doc.h -. y) *. doc.k)
      (width *. doc.k) ((-.height) *. doc.k) op
    | Some radius ->
      let scale = doc.k in
      let arc x1 y1 x2 y2 x3 y3 = print_buffer doc "%.2f %.2f %.2f %.2f %.2f %.2f c \n"
        (x1 *. scale) ((doc.h -. y1) *. scale)
        (x2 *. scale) ((doc.h -. y2) *. scale)
        (x3 *. scale) ((doc.h -. y3) *. scale) in
      let hp = doc.h in
      let my_arc = 4./.3. *. (sqrt 2. -. 1.) in
      print_buffer doc "%.2f %.2f m\n" ((x +. radius) *. scale) ((hp -. y) *. scale);
      let xc = x +. width -. radius in
      let yc = y +. radius in
      print_buffer doc "%.2f %.2f l\n" (xc *. scale) ((hp -. y) *. scale);
      arc (xc +. radius *. my_arc) (yc -. radius) (xc +. radius) (yc -. radius *. my_arc) (xc +. radius) yc;
      let xc = x +. width -. radius in
      let yc = y +. height -. radius in
      print_buffer doc "%.2f %.2f l\n" ((x +. width) *. scale) ((hp -. yc) *. scale);
      arc (xc +. radius) (yc +. radius *. my_arc) (xc +. radius *. my_arc) (yc +. radius) xc (yc +. radius);
      let xc = x +. radius in
      let yc = y +. height -. radius in
      print_buffer doc "%.2f %.2f l\n" (xc *. scale) ((hp -. (y +. height)) *. scale);
      arc (xc -. radius *. my_arc) (yc +. radius) (xc -. radius) (yc +. radius *. my_arc) (xc -. radius) yc;
      let xc = x +. radius in
      let yc = y +. radius in
      print_buffer doc "%.2f %.2f l\n" (x *. scale) ((hp -. yc) *. scale);
      arc (xc -. radius) (yc -. radius *. my_arc) (xc -. radius *. my_arc) (yc -. radius) xc  (yc -. radius);
      print_buffer doc "%c\n" op;;
