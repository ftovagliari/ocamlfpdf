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

open PDF

class table ~x ~y ~width ~height ~rows ~columns ?(spacing=0.) ?(padding=0.) doc =
  let n, m = float columns, float rows in
  let width = (width -. spacing *. (n -. 1.)) /. n in
  let height = (height -. spacing *. (m -. 1.)) /. m in
  let x0, y0 = x, y in
  let callbacks = Array.make_matrix rows columns (fun ~x ~y ~width ~height -> ()) in
  object (self)
    method x = x
    method y = y
    method pack () =
      for i = 0 to rows - 1 do
        for j = 0 to columns - 1 do
          let x = x0 +. (width +. spacing) *. (float j) in
          let y = y0 +. (height +. spacing) *. (float i) in
          PDF.rect ~x ~y ~width ~height (*~radius:5.0*) ~style:`Outline doc;
          let x = x +. padding in
          let y = y +. padding in
          let width = width -. 2. *. padding in
          let height = height -. 2. *. padding in
          callbacks.(i).(j) ~x ~y ~width ~height;
        done;
      done
    method set row column callback =
      callbacks.(row).(column) <- callback
  end

class virtual box ~x ~y ~width ~height ?(spacing=0.) ?(padding=0.) ?(border=false) doc =
  object (self)
    val mutable callbacks = ([] : (x:float -> y:float -> width:float -> height:float -> float) list)
    method x : float = x
    method y : float = y
    method width : float = width
    method height : float = height
    method add cb = callbacks <- cb :: callbacks
    method virtual pack : unit -> unit
  end

class vbox ~x ~y ~width ~height ?(spacing=0.) ?(padding=0.) ?(border=false) doc =
  object (self)
    inherit box ~x ~y ~width ~height ~spacing ~padding doc
    method padding = padding
    method spacing = spacing
    method get_homogeneous_child_height n =
      let n = float n in
      (width -. 2. *. padding -. (n -. 1.) *. spacing) /. n
    method pack () =
      let cbs = Array.of_list (List.rev callbacks) in
      let length = float (Array.length cbs) in
      if border then begin
        PDF.set_draw_color ~red:255 ~green:0 ~blue:0 doc;
        PDF.rect ~x ~y ~width ~height ~style:`Outline doc;
        PDF.set_draw_color ~red:0 ~green:0 ~blue:0 doc;
      end;
      let x = (x +. padding) in
      let y = y +. padding in
      let width = width -. 2. *. padding in
      let height = height -. (length -. 1.) *. spacing -. 2. *. padding in
      ignore (Array.fold_left begin fun (y, height) cb ->
        PDF.set ~x ~y doc;
        let previous_h = cb ~x ~y ~width ~height in
        let y = y +. previous_h +. spacing in
        let height = height -. previous_h in
        (y, height)
      end (y, height) cbs)
  end

class hbox ~x ~y ~width ~height ?(spacing=0.) ?(padding=0.) ?(border=false) doc =
  object (self)
    inherit box ~x ~y ~width ~height ~spacing ~padding doc
    method padding = padding
    method spacing = spacing
    method get_homogeneous_child_width n =
      let n = float n in
      (width -. 2. *. padding -. (n -. 1.) *. spacing) /. n
    method pack () =
      let cbs = Array.of_list (List.rev callbacks) in
      let length = float (Array.length cbs) in
      if border then begin
        PDF.set_draw_color ~red:0 ~green:255 ~blue:0 doc;
        PDF.rect ~x ~y ~width ~height ~style:`Outline doc;
        PDF.set_draw_color ~red:0 ~green:0 ~blue:0 doc;
      end;
      let x = x +. padding in
      let y = y +. padding in
      let width = width -. (length -. 1.) *. spacing -. 2. *. padding in
      let height = height -. 2. *. padding in
      ignore (Array.fold_left begin fun (x, width) cb ->
        PDF.set ~x ~y doc;
        let previous_w = cb ~x ~y ~width ~height in
        let x = x +. previous_w +. spacing in
        let width = width -. previous_w in
        x, width
      end (x, width) cbs)
  end


