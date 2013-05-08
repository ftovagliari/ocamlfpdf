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


open Font_metrics

let descriptor = {
  fontType           = `Core;
  fontName           = "Helvetica-Bold";
  fontBBox           = -170, -228, 1003, 962; (* lowerLeftX, lowerLeftY, upperRightX, upperRightY *)
  underlinePosition  = -100;
  underlineThickness = 50;
  charMetrics        = function
    | '\000'..'\031' -> 278 | ' ' -> 278 | '!' -> 333 | '"' -> 474 | '#' -> 556
    |'$' -> 556 | '%' -> 889 | '&' -> 722 | '\'' -> 238 | '(' -> 333 | ')' -> 333
    |'*' -> 389 | '+' -> 584 | ',' -> 278 | '-' -> 333 | '.' -> 278 | '/' -> 278 |
      '0' -> 556 | '1' -> 556 | '2' -> 556 | '3' -> 556 | '4' -> 556 | '5' -> 556 |
      '6' -> 556 | '7' -> 556 | '8' -> 556 | '9' -> 556 | ':' -> 333 | ';' -> 333 |
      '<' -> 584 | '=' -> 584 | '>' -> 584 | '?' -> 611 | '@' -> 975 | 'A' -> 722 |
      'B' -> 722 | 'C' -> 722 | 'D' -> 722 | 'E' -> 667 | 'F' -> 611 | 'G' -> 778 |
      'H' -> 722 | 'I' -> 278 | 'J' -> 556 | 'K' -> 722 | 'L' -> 611 | 'M' -> 833 |
      'N' -> 722 | 'O' -> 778 | 'P' -> 667 | 'Q' -> 778 | 'R' -> 722 | 'S' -> 667 |
      'T' -> 611 | 'U' -> 722 | 'V' -> 667 | 'W' -> 944 | 'X' -> 667 | 'Y' -> 667 |
      'Z' -> 611 | '[' -> 333 | '\\' -> 278 | ']' -> 333 | '^' -> 584 | '_' -> 556 |
      '`' -> 333 | 'a' -> 556 | 'b' -> 611 | 'c' -> 556 | 'd' -> 611 | 'e' -> 556 |
      'f' -> 333 | 'g' -> 611 | 'h' -> 611 | 'i' -> 278 | 'j' -> 278 | 'k' -> 556 |
      'l' -> 278 | 'm' -> 889 | 'n' -> 611 | 'o' -> 611 | 'p' -> 611 | 'q' -> 611 |
      'r' -> 389 | 's' -> 556 | 't' -> 333 | 'u' -> 611 | 'v' -> 556 | 'w' -> 778 |
      'x' -> 556 | 'y' -> 556 | 'z' -> 500 | '{' -> 389 | '|' -> 280 | '}' -> 389 |
      '~' -> 584 | '\127' -> 350 | '\128' -> 556 | '\129' -> 350 | '\130' -> 278 |
      '\131' -> 556 | '\132' -> 500 | '\133' -> 1000 | '\134' -> 556 | '\135' -> 556 |
      '\136' -> 333 | '\137' -> 1000 | '\138' -> 667 | '\139' -> 333 | '\140' -> 1000
    | '\141' -> 350 | '\142' -> 611 | '\143' -> 350 | '\144' -> 350 | '\145' -> 278
    | '\146' -> 278 | '\147' -> 500 | '\148' -> 500 | '\149' -> 350 | '\150' -> 556
    | '\151' -> 1000 | '\152' -> 333 | '\153' -> 1000 | '\154' -> 556 | '\155' ->
      333 | '\156' -> 944 | '\157' -> 350 | '\158' -> 500 | '\159' -> 667 | '\160' ->
      278 | '\161' -> 333 | '\162' -> 556 | '\163' -> 556 | '\164' -> 556 | '\165' ->
      556 | '\166' -> 280 | '\167' -> 556 | '\168' -> 333 | '\169' -> 737 | '\170' ->
      370 | '\171' -> 556 | '\172' -> 584 | '\173' -> 333 | '\174' -> 737 | '\175' ->
      333 | '\176' -> 400 | '\177' -> 584 | '\178' -> 333 | '\179' -> 333 | '\180' ->
      333 | '\181' -> 611 | '\182' -> 556 | '\183' -> 278 | '\184' -> 333 | '\185' ->
      333 | '\186' -> 365 | '\187' -> 556 | '\188' -> 834 | '\189' -> 834 | '\190' ->
      834 | '\191' -> 611 | '\192' -> 722 | '\193' -> 722 | '\194' -> 722 | '\195' ->
      722 | '\196' -> 722 | '\197' -> 722 | '\198' -> 1000 | '\199' -> 722 | '\200' ->
      667 | '\201' -> 667 | '\202' -> 667 | '\203' -> 667 | '\204' -> 278 | '\205' ->
      278 | '\206' -> 278 | '\207' -> 278 | '\208' -> 722 | '\209' -> 722 | '\210' ->
      778 | '\211' -> 778 | '\212' -> 778 | '\213' -> 778 | '\214' -> 778 | '\215' ->
      584 | '\216' -> 778 | '\217' -> 722 | '\218' -> 722 | '\219' -> 722 | '\220' ->
      722 | '\221' -> 667 | '\222' -> 667 | '\223' -> 611 | '\224' -> 556 | '\225' ->
      556 | '\226' -> 556 | '\227' -> 556 | '\228' -> 556 | '\229' -> 556 | '\230' ->
      889 | '\231' -> 556 | '\232' -> 556 | '\233' -> 556 | '\234' -> 556 | '\235' ->
      556 | '\236' -> 278 | '\237' -> 278 | '\238' -> 278 | '\239' -> 278 | '\240' ->
      611 | '\241' -> 611 | '\242' -> 611 | '\243' -> 611 | '\244' -> 611 | '\245' ->
      611 | '\246' -> 611 | '\247' -> 584 | '\248' -> 611 | '\249' -> 611 | '\250' ->
      611 | '\251' -> 611 | '\252' -> 611 | '\253' -> 556 | '\254' -> 611 | '\255' -> 556
}
