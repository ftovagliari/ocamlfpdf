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

open PDF

class table ~x ~y ~width ~height ~rows ~columns ?(spacing=0.) ?(padding=0.) doc =
  let n, m = float columns, float rows in
  let width = (width -. spacing *. (n -. 1.)) /. n in
  let height = (height -. spacing *. (m -. 1.)) /. m in
  let x0, y0 = x, y in
  let callbacks = Array.make_matrix rows columns (fun ~x ~y ~width ~height -> ()) in
  object (self)
    method set row column callback =
      callbacks.(row).(column) <- callback
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
  end

class virtual box ~x ~y ~width ~height ?(spacing=0.) ?(padding=0.) ?(border=false) doc =
  object (self)
    val mutable callbacks = ([] : (x:float -> y:float -> width:float -> height:float -> float) list)
    method add cb = callbacks <- cb :: callbacks
    method virtual pack : unit -> unit
  end

class vbox ~x ~y ~width ~height ?(spacing=0.) ?(padding=0.) ?(border=false) doc =
  object (self)
    inherit box ~x ~y ~width ~height ~spacing ~padding doc
    method dim () = width
    method pack () =
      let cbs = Array.of_list (List.rev callbacks) in
      let length = float (Array.length cbs) in
      if border then begin
        PDF.set_draw_color ~red:255 ~green:0 ~blue:0 doc;
        PDF.rect ~x ~y ~width ~height ~style:`Outline doc;
        PDF.set_draw_color ~red:0 ~green:0 ~blue:0 doc;
      end;
      let x = (x +. padding) in
      let y = ref (y +. padding) in
      let width = width -. 2. *. padding in
      let height = ref (height -. (length -. 1.) *. spacing -. 2. (* *. length*) *. padding) in
      Array.iter begin fun cb ->
        PDF.set ~x ~y:!y doc;
        let previous_h = cb ~x ~y:!y ~width ~height:!height in
        y := !y +. previous_h +. spacing (* +. 2. *. padding*);
        height := !height -. previous_h;
      end cbs
  end

class hbox ~x ~y ~width ~height ?(spacing=0.) ?(padding=0.) ?(border=false) doc =
  object (self)
    inherit box ~x ~y ~width ~height ~spacing ~padding doc
    method pack () =
      let cbs = Array.of_list (List.rev callbacks) in
      let length = float (Array.length cbs) in
      if border then begin
        PDF.set_draw_color ~red:0 ~green:255 ~blue:0 doc;
        PDF.rect ~x ~y ~width ~height ~style:`Outline doc;
        PDF.set_draw_color ~red:0 ~green:0 ~blue:0 doc;
      end;
      let x = ref (x +. padding) in
      let y = (y +. padding) in
      let width = width -. (length -. 1.) *. spacing -. 2. (* *. length*) *. padding in
      let height = height -. 2. *. padding in
      Array.iter begin fun cb ->
        PDF.set ~x:!x ~y doc;
        let previous_w = cb ~x:!x ~y ~width ~height in
        x := !x +. previous_w +. spacing(* +. 2. *. padding*);
      end cbs
  end


