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
  fontName           = "Times-Roman";
  fontFamily         = `Times;
  fontWeight         = 400;
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
  fontBBox           = -168, -218, 1000, 898; (* lowerLeftX, lowerLeftY, upperRightX, upperRightY *)
  underlinePosition  = -100;
  underlineThickness = 50;
  charMetrics        = function
    | '\000'..'\031' -> 250 | ' ' -> 250 | '!' -> 333 | '"' -> 408 | '#' -> 500 |
      '$' -> 500 | '%' -> 833 | '&' -> 778 | '\'' -> 180 | '(' -> 333 | ')' -> 333 |
      '*' -> 500 | '+' -> 564 | '-' -> 333 | ',' -> 250 | '.' -> 250 | '/' -> 278 |
      '0'..'9' -> 500 | ':' -> 278 | ';' -> 278 | '<' -> 564 | '=' -> 564 | '>' -> 564
    | '?' -> 444 | '@' -> 921 | 'A' -> 722 | 'B' -> 667 | 'C' -> 667 | 'D' -> 722 |
      'E' -> 611 | 'F' -> 556 | 'G' -> 722 | 'H' -> 722 | 'I' -> 333 | 'J' -> 389 |
      'K' -> 722 | 'L' -> 611 | 'M' -> 889 | 'N' -> 722 | 'O' -> 722 | 'P' -> 556 |
      'Q' -> 722 | 'R' -> 667 | 'S' -> 556 | 'T' -> 611 | 'U' -> 722 | 'V' -> 722 |
      'W' -> 944 | 'X' -> 722 | 'Y' -> 722 | 'Z' -> 611 | '[' -> 333 | '\\' -> 278 |
      ']' -> 333 | '^' -> 469 | '_' -> 500 | '`' -> 333 | 'a' -> 444 | 'b' -> 500 |
      'c' -> 444 | 'd' -> 500 | 'e' -> 444 | 'f' -> 333 | 'g' -> 500 | 'h' -> 500 |
      'i' -> 278 | 'j' -> 278 | 'k' -> 500 | 'l' -> 278 | 'm' -> 778 | 'n' -> 500 |
      'o' -> 500 | 'p' -> 500 | 'q' -> 500 | 'r' -> 333 | 's' -> 389 | 't' -> 278 |
      'u' -> 500 | 'v' -> 500 | 'w' -> 722 | 'x' -> 500 | 'y' -> 500 | 'z' -> 444 |
      '{' -> 480 | '|' -> 200 | '}' -> 480 | '~' -> 541 | '\127' -> 350 | '\128' ->
      500 | '\129' -> 350 | '\130' -> 333 | '\131' -> 500 | '\132' -> 444 | '\133' ->
      1000 | '\134' -> 500 | '\135' -> 500 | '\136' -> 333 | '\137' -> 1000 | '\138'
      -> 556 | '\139' -> 333 | '\140' -> 889 | '\141' -> 350 | '\142' -> 611 | '\143'
      -> 350 | '\144' -> 350 | '\145' -> 333 | '\146' -> 333 | '\147' -> 444 | '\148'
      -> 444 | '\149' -> 350 | '\150' -> 500 | '\151' -> 1000 | '\152' -> 333 | '\153'
      -> 980 | '\154' -> 389 | '\155' -> 333 | '\156' -> 722 | '\157' -> 350 | '\158'
      -> 444 | '\159' -> 722 | '\160' -> 250 | '\161' -> 333 | '\162' -> 500 | '\163'
      -> 500 | '\164' -> 500 | '\165' -> 500 | '\166' -> 200 | '\167' -> 500 | '\168'
      -> 333 | '\169' -> 760 | '\170' -> 276 | '\171' -> 500 | '\172' -> 564 | '\173'
      -> 333 | '\174' -> 760 | '\175' -> 333 | '\176' -> 400 | '\177' -> 564 | '\178'
      -> 300 | '\179' -> 300 | '\180' -> 333 | '\181' -> 500 | '\182' -> 453 | '\183'
      -> 250 | '\184' -> 333 | '\185' -> 300 | '\186' -> 310 | '\187' -> 500 | '\188'
      -> 750 | '\189' -> 750 | '\190' -> 750 | '\191' -> 444 | '\192' -> 722 | '\193'
      -> 722 | '\194' -> 722 | '\195' -> 722 | '\196' -> 722 | '\197' -> 722 | '\198'
      -> 889 | '\199' -> 667 | '\200' -> 611 | '\201' -> 611 | '\202' -> 611 | '\203'
      -> 611 | '\204' -> 333 | '\205' -> 333 | '\206' -> 333 | '\207' -> 333 | '\208'
      -> 722 | '\209' -> 722 | '\210' -> 722 | '\211' -> 722 | '\212' -> 722 | '\213'
      -> 722 | '\214' -> 722 | '\215' -> 564 | '\216' -> 722 | '\217' -> 722 | '\218'
      -> 722 | '\219' -> 722 | '\220' -> 722 | '\221' -> 722 | '\222' -> 556 | '\223'
      -> 500 | '\224' -> 444 | '\225' -> 444 | '\226' -> 444 | '\227' -> 444 | '\228'
      -> 444 | '\229' -> 444 | '\230' -> 667 | '\231' -> 444 | '\232' -> 444 | '\233'
      -> 444 | '\234' -> 444 | '\235' -> 444
    | '\236' -> 278 | '\237' -> 278 | '\238' -> 278 | '\239' -> 278 | '\240' -> 500
    | '\241' -> 500 | '\242' -> 500 | '\243' -> 500 | '\244' -> 500 | '\245' -> 500
    | '\246' -> 500 | '\247' -> 564 | '\248' -> 500 | '\249' -> 500 | '\250' -> 500
    | '\251' -> 500 | '\252' -> 500 | '\253' -> 500 | '\254' -> 500 | '\255' -> 500
}
