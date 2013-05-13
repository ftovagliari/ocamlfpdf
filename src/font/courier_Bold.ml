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


open Font

let descriptor = {
  fontType           = `Core;
  fontName           = "Courier-Bold";
  fontFamily         = `Courier;
  fontWeight         = 700;
  flags              = None;
  italicAngle        = 0.;
  ascent             = None;
  descent            = None;
  capHeight          = None;
  stemV              = None;
  missingWidth       = None;
  fontFile           = None;
  diff	             = None;
  encoding           = None;
  fontBBox           = -113, -250, 749, 801; (* lowerLeftX, lowerLeftY, upperRightX, upperRightY *)
  underlinePosition  = -100;
  underlineThickness = 50;
  charMetrics        = function _ -> 600;
}
