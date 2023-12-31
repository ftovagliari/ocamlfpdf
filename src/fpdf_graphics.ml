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

open Fpdf_types
open Fpdf_document
open Printf

(** draw_color *)
let set_draw_color ~red ?(green=(-1)) ?(blue=(-1)) doc =
  if red = green && green = blue || green = -1 then
    doc.drawColor <- if red = 0 then "0 G" else sprintf "%.3f G" ((float red) /. 255.)
  else begin doc.drawColor <- sprintf "%.3f %.3f %.3f RG"
    ((float red) /. 255.) ((float green) /. 255.) ((float blue) /. 255.)
  end;
  if doc.page >= 0 then print_buffer doc "%s\n" doc.drawColor;
  doc.draw_color_rgb <- (red, green, blue)

let draw_color doc = doc.draw_color_rgb

(** fill_color *)
let set_fill_color ~red ?(green=(-1)) ?(blue=(-1)) doc =
  if red = green && green = blue || green = -1 then
    doc.fillColor <- if red = 0 then "0 g" else sprintf "%.3f g" ((float red) /. 255.)
  else begin doc.fillColor <- sprintf "%.3f %.3f %.3f rg"
    ((float red) /. 255.) ((float green) /. 255.) ((float blue) /. 255.)
  end;
  if doc.page >= 0 then print_buffer doc "%s\n" doc.fillColor;
  doc.fill_color_rgb <- (red, green, blue)

let fill_color doc = doc.fill_color_rgb

(** image *)
let image ~name ~data ~x ~y ?(width=0.) ?(height=0.) ?link doc =
  let open Fpdf_images in
  let info =
    match Fpdf_images.Table.find name doc.images with
      | Some x -> x
      | None -> begin
          let info = Fpdf_images.parse data in
          info.image_name <- name;
           Fpdf_images.Table.add name info doc.images;
          info
        end
  in
  (*    let width, height = swidth, sheight in*)
  let width, height = match width, height with
      | 0., 0. -> ((float info.image_width) /. doc.k), ((float info.image_height) /. doc.k)
      | _ -> width, height in
    let width = if width = 0. then height *. (float info.image_width) /. (float info.image_height)
      else width in
    let height = if height = 0. then width *. (float info.image_height) /. (float info.image_width)
      else height in
    print_buffer doc "q %f 0 0 %f %f %f cm /I%d Do Q\n" (width *. doc.k) (height *. doc.k)
      (x *. doc.k) ((doc.h -. (y +. height)) *. doc.k) info.image_index;
    match link with
      | None -> ()
      | Some l -> add_link ~x ~y ~width ~height ~link:l ();;

(** line *)
let line ~x1 ~y1 ~x2 ~y2 doc =
  print_buffer doc "%f %f m %f %f l S\n"
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
    | None -> print_buffer doc "%f %f %f %f re %c\n"
      (x *. doc.k) ((doc.h -. y) *. doc.k)
      (width *. doc.k) ((-.height) *. doc.k) op
    | Some (r1, r2, r3, r4) ->
      let scale = doc.k in
      let arc x1 y1 x2 y2 x3 y3 = print_buffer doc "%f %f %f %f %f %f c \n"
        (x1 *. scale) ((doc.h -. y1) *. scale)
        (x2 *. scale) ((doc.h -. y2) *. scale)
        (x3 *. scale) ((doc.h -. y3) *. scale) in
      let hp = doc.h in
      let my_arc = 4./.3. *. (sqrt 2. -. 1.) in
      print_buffer doc "%f %f m\n" ((x +. r4) *. scale) ((hp -. y) *. scale);
      let xc = x +. width -. r1 in
      let yc = y +. r1 in
      print_buffer doc "%f %f l\n" (xc *. scale) ((hp -. y) *. scale);
      arc (xc +. r1 *. my_arc) (yc -. r1) (xc +. r1) (yc -. r1 *. my_arc) (xc +. r1) yc;
      let xc = x +. width -. r2 in
      let yc = y +. height -. r2 in
      print_buffer doc "%f %f l\n" ((x +. width) *. scale) ((hp -. yc) *. scale);
      arc (xc +. r2) (yc +. r2 *. my_arc) (xc +. r2 *. my_arc) (yc +. r2) xc (yc +. r2);
      let xc = x +. r3 in
      let yc = y +. height -. r3 in
      print_buffer doc "%f %f l\n" (xc *. scale) ((hp -. (y +. height)) *. scale);
      arc (xc -. r3 *. my_arc) (yc +. r3) (xc -. r3) (yc +. r3 *. my_arc) (xc -. r3) yc;
      let xc = x +. r4 in
      let yc = y +. r4 in
      print_buffer doc "%f %f l\n" (x *. scale) ((hp -. yc) *. scale);
      arc (xc -. r4) (yc -. r4 *. my_arc) (xc -. r4 *. my_arc) (yc -. r4) xc  (yc -. r4);
      print_buffer doc "%c\n" op;;
