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


(*module type FONT =
sig
  val normal : char -> int
  val bold : char -> int
  val italic : char -> int
  val bold_italic : char -> int
end*)

module Courier (*: FONT*) =
struct
  let normal = function _ -> 600
  let bold = normal
  let italic = normal
  let bold_italic = normal
end

module Helvetica (*: FONT*) =
struct
  let normal = function '\000'..'\031' -> 278 | ' ' -> 278 | '!' -> 278 | '"' -> 355 | '#' -> 556 | '$' -> 556 | '%' -> 889 | '&' -> 667 | '\'' -> 191 | '(' -> 333 | ')' -> 333 | '*' -> 389 | '+' -> 584 | ',' -> 278 | '-' -> 333 | '.' -> 278 | '/' -> 278 | '0' -> 556 | '1' -> 556 | '2' -> 556 | '3' -> 556 | '4' -> 556 | '5' -> 556 | '6' -> 556 | '7' -> 556 | '8' -> 556 | '9' -> 556 | ':' -> 278 | ';' -> 278 | '<' -> 584 | '=' -> 584 | '>' -> 584 | '?' -> 556 | '@' -> 1015 | 'A' -> 667 | 'B' -> 667 | 'C' -> 722 | 'D' -> 722 | 'E' -> 667 | 'F' -> 611 | 'G' -> 778 | 'H' -> 722 | 'I' -> 278 | 'J' -> 500 | 'K' -> 667 | 'L' -> 556 | 'M' -> 833 | 'N' -> 722 | 'O' -> 778 | 'P' -> 667 | 'Q' -> 778 | 'R' -> 722 | 'S' -> 667 | 'T' -> 611 | 'U' -> 722 | 'V' -> 667 | 'W' -> 944 | 'X' -> 667 | 'Y' -> 667 | 'Z' -> 611 | '[' -> 278 | '\\' -> 278 | ']' -> 278 | '^' -> 469 | '_' -> 556 | '`' -> 333 | 'a' -> 556 | 'b' -> 556 | 'c' -> 500 | 'd' -> 556 | 'e' -> 556 | 'f' -> 278 | 'g' -> 556 | 'h' -> 556 | 'i' -> 222 | 'j' -> 222 | 'k' -> 500 | 'l' -> 222 | 'm' -> 833 | 'n' -> 556 | 'o' -> 556 | 'p' -> 556 | 'q' -> 556 | 'r' -> 333 | 's' -> 500 | 't' -> 278 | 'u' -> 556 | 'v' -> 500 | 'w' -> 722 | 'x' -> 500 | 'y' -> 500 | 'z' -> 500 | '{' -> 334 | '|' -> 260 | '}' -> 334 | '~' -> 584 | '\127' -> 350 | '\128' -> 556 | '\129' -> 350 | '\130' -> 222 | '\131' -> 556 | '\132' -> 333 | '\133' -> 1000 | '\134' -> 556 | '\135' -> 556 | '\136' -> 333 | '\137' -> 1000 | '\138' -> 667 | '\139' -> 333 | '\140' -> 1000 | '\141' -> 350 | '\142' -> 611 | '\143' -> 350 | '\144' -> 350 | '\145' -> 222 | '\146' -> 222 | '\147' -> 333 | '\148' -> 333 | '\149' -> 350 | '\150' -> 556 | '\151' -> 1000 | '\152' -> 333 | '\153' -> 1000 | '\154' -> 500 | '\155' -> 333 | '\156' -> 944 | '\157' -> 350 | '\158' -> 500 | '\159' -> 667 | '\160' -> 278 | '\161' -> 333 | '\162' -> 556 | '\163' -> 556 | '\164' -> 556 | '\165' -> 556 | '\166' -> 260 | '\167' -> 556 | '\168' -> 333 | '\169' -> 737 | '\170' -> 370 | '\171' -> 556 | '\172' -> 584 | '\173' -> 333 | '\174' -> 737 | '\175' -> 333 | '\176' -> 400 | '\177' -> 584 | '\178' -> 333 | '\179' -> 333 | '\180' -> 333 | '\181' -> 556 | '\182' -> 537 | '\183' -> 278 | '\184' -> 333 | '\185' -> 333 | '\186' -> 365 | '\187' -> 556 | '\188' -> 834 | '\189' -> 834 | '\190' -> 834 | '\191' -> 611 | '\192' -> 667 | '\193' -> 667 | '\194' -> 667 | '\195' -> 667 | '\196' -> 667 | '\197' -> 667 | '\198' -> 1000 | '\199' -> 722 | '\200' -> 667 | '\201' -> 667 | '\202' -> 667 | '\203' -> 667 | '\204' -> 278 | '\205' -> 278 | '\206' -> 278 | '\207' -> 278 | '\208' -> 722 | '\209' -> 722 | '\210' -> 778 | '\211' -> 778 | '\212' -> 778 | '\213' -> 778 | '\214' -> 778 | '\215' -> 584 | '\216' -> 778 | '\217' -> 722 | '\218' -> 722 | '\219' -> 722 | '\220' -> 722 | '\221' -> 667 | '\222' -> 667 | '\223' -> 611 | '\224' -> 556 | '\225' -> 556 | '\226' -> 556 | '\227' -> 556 | '\228' -> 556 | '\229' -> 556 | '\230' -> 889 | '\231' -> 500 | '\232' -> 556 | '\233' -> 556 | '\234' -> 556 | '\235' -> 556 | '\236' -> 278 | '\237' -> 278 | '\238' -> 278 | '\239' -> 278 | '\240' -> 556 | '\241' -> 556 | '\242' -> 556 | '\243' -> 556 | '\244' -> 556 | '\245' -> 556 | '\246' -> 556 | '\247' -> 584 | '\248' -> 611 | '\249' -> 556 | '\250' -> 556 | '\251' -> 556 | '\252' -> 556 | '\253' -> 500 | '\254' -> 556 | '\255' -> 500
  let bold = function '\000'..'\031' -> 278 | ' ' -> 278 | '!' -> 333 | '"' -> 474 | '#' -> 556 | '$' -> 556 | '%' -> 889 | '&' -> 722 | '\'' -> 238 | '(' -> 333 | ')' -> 333 | '*' -> 389 | '+' -> 584 | ',' -> 278 | '-' -> 333 | '.' -> 278 | '/' -> 278 | '0' -> 556 | '1' -> 556 | '2' -> 556 | '3' -> 556 | '4' -> 556 | '5' -> 556 | '6' -> 556 | '7' -> 556 | '8' -> 556 | '9' -> 556 | ':' -> 333 | ';' -> 333 | '<' -> 584 | '=' -> 584 | '>' -> 584 | '?' -> 611 | '@' -> 975 | 'A' -> 722 | 'B' -> 722 | 'C' -> 722 | 'D' -> 722 | 'E' -> 667 | 'F' -> 611 | 'G' -> 778 | 'H' -> 722 | 'I' -> 278 | 'J' -> 556 | 'K' -> 722 | 'L' -> 611 | 'M' -> 833 | 'N' -> 722 | 'O' -> 778 | 'P' -> 667 | 'Q' -> 778 | 'R' -> 722 | 'S' -> 667 | 'T' -> 611 | 'U' -> 722 | 'V' -> 667 | 'W' -> 944 | 'X' -> 667 | 'Y' -> 667 | 'Z' -> 611 | '[' -> 333 | '\\' -> 278 | ']' -> 333 | '^' -> 584 | '_' -> 556 | '`' -> 333 | 'a' -> 556 | 'b' -> 611 | 'c' -> 556 | 'd' -> 611 | 'e' -> 556 | 'f' -> 333 | 'g' -> 611 | 'h' -> 611 | 'i' -> 278 | 'j' -> 278 | 'k' -> 556 | 'l' -> 278 | 'm' -> 889 | 'n' -> 611 | 'o' -> 611 | 'p' -> 611 | 'q' -> 611 | 'r' -> 389 | 's' -> 556 | 't' -> 333 | 'u' -> 611 | 'v' -> 556 | 'w' -> 778 | 'x' -> 556 | 'y' -> 556 | 'z' -> 500 | '{' -> 389 | '|' -> 280 | '}' -> 389 | '~' -> 584 | '\127' -> 350 | '\128' -> 556 | '\129' -> 350 | '\130' -> 278 | '\131' -> 556 | '\132' -> 500 | '\133' -> 1000 | '\134' -> 556 | '\135' -> 556 | '\136' -> 333 | '\137' -> 1000 | '\138' -> 667 | '\139' -> 333 | '\140' -> 1000 | '\141' -> 350 | '\142' -> 611 | '\143' -> 350 | '\144' -> 350 | '\145' -> 278 | '\146' -> 278 | '\147' -> 500 | '\148' -> 500 | '\149' -> 350 | '\150' -> 556 | '\151' -> 1000 | '\152' -> 333 | '\153' -> 1000 | '\154' -> 556 | '\155' -> 333 | '\156' -> 944 | '\157' -> 350 | '\158' -> 500 | '\159' -> 667 | '\160' -> 278 | '\161' -> 333 | '\162' -> 556 | '\163' -> 556 | '\164' -> 556 | '\165' -> 556 | '\166' -> 280 | '\167' -> 556 | '\168' -> 333 | '\169' -> 737 | '\170' -> 370 | '\171' -> 556 | '\172' -> 584 | '\173' -> 333 | '\174' -> 737 | '\175' -> 333 | '\176' -> 400 | '\177' -> 584 | '\178' -> 333 | '\179' -> 333 | '\180' -> 333 | '\181' -> 611 | '\182' -> 556 | '\183' -> 278 | '\184' -> 333 | '\185' -> 333 | '\186' -> 365 | '\187' -> 556 | '\188' -> 834 | '\189' -> 834 | '\190' -> 834 | '\191' -> 611 | '\192' -> 722 | '\193' -> 722 | '\194' -> 722 | '\195' -> 722 | '\196' -> 722 | '\197' -> 722 | '\198' -> 1000 | '\199' -> 722 | '\200' -> 667 | '\201' -> 667 | '\202' -> 667 | '\203' -> 667 | '\204' -> 278 | '\205' -> 278 | '\206' -> 278 | '\207' -> 278 | '\208' -> 722 | '\209' -> 722 | '\210' -> 778 | '\211' -> 778 | '\212' -> 778 | '\213' -> 778 | '\214' -> 778 | '\215' -> 584 | '\216' -> 778 | '\217' -> 722 | '\218' -> 722 | '\219' -> 722 | '\220' -> 722 | '\221' -> 667 | '\222' -> 667 | '\223' -> 611 | '\224' -> 556 | '\225' -> 556 | '\226' -> 556 | '\227' -> 556 | '\228' -> 556 | '\229' -> 556 | '\230' -> 889 | '\231' -> 556 | '\232' -> 556 | '\233' -> 556 | '\234' -> 556 | '\235' -> 556 | '\236' -> 278 | '\237' -> 278 | '\238' -> 278 | '\239' -> 278 | '\240' -> 611 | '\241' -> 611 | '\242' -> 611 | '\243' -> 611 | '\244' -> 611 | '\245' -> 611 | '\246' -> 611 | '\247' -> 584 | '\248' -> 611 | '\249' -> 611 | '\250' -> 611 | '\251' -> 611 | '\252' -> 611 | '\253' -> 556 | '\254' -> 611 | '\255' -> 556
  let italic = function      '\000'..'\031' -> 278 | ' ' -> 278 | '!' -> 278 | '"' -> 355 | '#' -> 556 | '$' -> 556 | '%' -> 889 | '&' -> 667 | '\'' -> 191 | '(' -> 333 | ')' -> 333 | '*' -> 389 | '+' -> 584 | ',' -> 278 | '-' -> 333 | '.' -> 278 | '/' -> 278 | '0' -> 556 | '1' -> 556 | '2' -> 556 | '3' -> 556 | '4' -> 556 | '5' -> 556 | '6' -> 556 | '7' -> 556 | '8' -> 556 | '9' -> 556 | ':' -> 278 | ';' -> 278 | '<' -> 584 | '=' -> 584 | '>' -> 584 | '?' -> 556 | '@' -> 1015 | 'A' -> 667 | 'B' -> 667 | 'C' -> 722 | 'D' -> 722 | 'E' -> 667 | 'F' -> 611 | 'G' -> 778 | 'H' -> 722 | 'I' -> 278 | 'J' -> 500 | 'K' -> 667 | 'L' -> 556 | 'M' -> 833 | 'N' -> 722 | 'O' -> 778 | 'P' -> 667 | 'Q' -> 778 | 'R' -> 722 | 'S' -> 667 | 'T' -> 611 | 'U' -> 722 | 'V' -> 667 | 'W' -> 944 | 'X' -> 667 | 'Y' -> 667 | 'Z' -> 611 | '[' -> 278 | '\\' -> 278 | ']' -> 278 | '^' -> 469 | '_' -> 556 | '`' -> 333 | 'a' -> 556 | 'b' -> 556 | 'c' -> 500 | 'd' -> 556 | 'e' -> 556 | 'f' -> 278 | 'g' -> 556 | 'h' -> 556 | 'i' -> 222 | 'j' -> 222 | 'k' -> 500 | 'l' -> 222 | 'm' -> 833 | 'n' -> 556 | 'o' -> 556 | 'p' -> 556 | 'q' -> 556 | 'r' -> 333 | 's' -> 500 | 't' -> 278 | 'u' -> 556 | 'v' -> 500 | 'w' -> 722 | 'x' -> 500 | 'y' -> 500 | 'z' -> 500 | '{' -> 334 | '|' -> 260 | '}' -> 334 | '~' -> 584 | '\127' -> 350 | '\128' -> 556 | '\129' -> 350 | '\130' -> 222 | '\131' -> 556 | '\132' -> 333 | '\133' -> 1000 | '\134' -> 556 | '\135' -> 556 | '\136' -> 333 | '\137' -> 1000 | '\138' -> 667 | '\139' -> 333 | '\140' -> 1000 | '\141' -> 350 | '\142' -> 611 | '\143' -> 350 | '\144' -> 350 | '\145' -> 222 | '\146' -> 222 | '\147' -> 333 | '\148' -> 333 | '\149' -> 350 | '\150' -> 556 | '\151' -> 1000 | '\152' -> 333 | '\153' -> 1000 | '\154' -> 500 | '\155' -> 333 | '\156' -> 944 | '\157' -> 350 | '\158' -> 500 | '\159' -> 667 | '\160' -> 278 | '\161' -> 333 | '\162' -> 556 | '\163' -> 556 | '\164' -> 556 | '\165' -> 556 | '\166' -> 260 | '\167' -> 556 | '\168' -> 333 | '\169' -> 737 | '\170' -> 370 | '\171' -> 556 | '\172' -> 584 | '\173' -> 333 | '\174' -> 737 | '\175' -> 333 | '\176' -> 400 | '\177' -> 584 | '\178' -> 333 | '\179' -> 333 | '\180' -> 333 | '\181' -> 556 | '\182' -> 537 | '\183' -> 278 | '\184' -> 333 | '\185' -> 333 | '\186' -> 365 | '\187' -> 556 | '\188' -> 834 | '\189' -> 834 | '\190' -> 834 | '\191' -> 611 | '\192' -> 667 | '\193' -> 667 | '\194' -> 667 | '\195' -> 667 | '\196' -> 667 | '\197' -> 667 | '\198' -> 1000 | '\199' -> 722 | '\200' -> 667 | '\201' -> 667 | '\202' -> 667 | '\203' -> 667 | '\204' -> 278 | '\205' -> 278 | '\206' -> 278 | '\207' -> 278 | '\208' -> 722 | '\209' -> 722 | '\210' -> 778 | '\211' -> 778 | '\212' -> 778 | '\213' -> 778 | '\214' -> 778 | '\215' -> 584 | '\216' -> 778 | '\217' -> 722 | '\218' -> 722 | '\219' -> 722 | '\220' -> 722 | '\221' -> 667 | '\222' -> 667 | '\223' -> 611 | '\224' -> 556 | '\225' -> 556 | '\226' -> 556 | '\227' -> 556 | '\228' -> 556 | '\229' -> 556 | '\230' -> 889 | '\231' -> 500 | '\232' -> 556 | '\233' -> 556 | '\234' -> 556 | '\235' -> 556 | '\236' -> 278 | '\237' -> 278 | '\238' -> 278 | '\239' -> 278 | '\240' -> 556 | '\241' -> 556 | '\242' -> 556 | '\243' -> 556 | '\244' -> 556 | '\245' -> 556 | '\246' -> 556 | '\247' -> 584 | '\248' -> 611 | '\249' -> 556 | '\250' -> 556 | '\251' -> 556 | '\252' -> 556 | '\253' -> 500 | '\254' -> 556 | '\255' -> 500
  let bold_italic = function '\000'..'\031' -> 278 | ' ' -> 278 | '!' -> 333 | '"' -> 474 | '#' -> 556 | '$' -> 556 | '%' -> 889 | '&' -> 722 | '\'' -> 238 | '(' -> 333 | ')' -> 333 | '*' -> 389 | '+' -> 584 | ',' -> 278 | '-' -> 333 | '.' -> 278 | '/' -> 278 | '0' -> 556 | '1' -> 556 | '2' -> 556 | '3' -> 556 | '4' -> 556 | '5' -> 556 | '6' -> 556 | '7' -> 556 | '8' -> 556 | '9' -> 556 | ':' -> 333 | ';' -> 333 | '<' -> 584 | '=' -> 584 | '>' -> 584 | '?' -> 611 | '@' -> 975 | 'A' -> 722 | 'B' -> 722 | 'C' -> 722 | 'D' -> 722 | 'E' -> 667 | 'F' -> 611 | 'G' -> 778 | 'H' -> 722 | 'I' -> 278 | 'J' -> 556 | 'K' -> 722 | 'L' -> 611 | 'M' -> 833 | 'N' -> 722 | 'O' -> 778 | 'P' -> 667 | 'Q' -> 778 | 'R' -> 722 | 'S' -> 667 | 'T' -> 611 | 'U' -> 722 | 'V' -> 667 | 'W' -> 944 | 'X' -> 667 | 'Y' -> 667 | 'Z' -> 611 | '[' -> 333 | '\\' -> 278 | ']' -> 333 | '^' -> 584 | '_' -> 556 | '`' -> 333 | 'a' -> 556 | 'b' -> 611 | 'c' -> 556 | 'd' -> 611 | 'e' -> 556 | 'f' -> 333 | 'g' -> 611 | 'h' -> 611 | 'i' -> 278 | 'j' -> 278 | 'k' -> 556 | 'l' -> 278 | 'm' -> 889 | 'n' -> 611 | 'o' -> 611 | 'p' -> 611 | 'q' -> 611 | 'r' -> 389 | 's' -> 556 | 't' -> 333 | 'u' -> 611 | 'v' -> 556 | 'w' -> 778 | 'x' -> 556 | 'y' -> 556 | 'z' -> 500 | '{' -> 389 | '|' -> 280 | '}' -> 389 | '~' -> 584 | '\127' -> 350 | '\128' -> 556 | '\129' -> 350 | '\130' -> 278 | '\131' -> 556 | '\132' -> 500 | '\133' -> 1000 | '\134' -> 556 | '\135' -> 556 | '\136' -> 333 | '\137' -> 1000 | '\138' -> 667 | '\139' -> 333 | '\140' -> 1000 | '\141' -> 350 | '\142' -> 611 | '\143' -> 350 | '\144' -> 350 | '\145' -> 278 | '\146' -> 278 | '\147' -> 500 | '\148' -> 500 | '\149' -> 350 | '\150' -> 556 | '\151' -> 1000 | '\152' -> 333 | '\153' -> 1000 | '\154' -> 556 | '\155' -> 333 | '\156' -> 944 | '\157' -> 350 | '\158' -> 500 | '\159' -> 667 | '\160' -> 278 | '\161' -> 333 | '\162' -> 556 | '\163' -> 556 | '\164' -> 556 | '\165' -> 556 | '\166' -> 280 | '\167' -> 556 | '\168' -> 333 | '\169' -> 737 | '\170' -> 370 | '\171' -> 556 | '\172' -> 584 | '\173' -> 333 | '\174' -> 737 | '\175' -> 333 | '\176' -> 400 | '\177' -> 584 | '\178' -> 333 | '\179' -> 333 | '\180' -> 333 | '\181' -> 611 | '\182' -> 556 | '\183' -> 278 | '\184' -> 333 | '\185' -> 333 | '\186' -> 365 | '\187' -> 556 | '\188' -> 834 | '\189' -> 834 | '\190' -> 834 | '\191' -> 611 | '\192' -> 722 | '\193' -> 722 | '\194' -> 722 | '\195' -> 722 | '\196' -> 722 | '\197' -> 722 | '\198' -> 1000 | '\199' -> 722 | '\200' -> 667 | '\201' -> 667 | '\202' -> 667 | '\203' -> 667 | '\204' -> 278 | '\205' -> 278 | '\206' -> 278 | '\207' -> 278 | '\208' -> 722 | '\209' -> 722 | '\210' -> 778 | '\211' -> 778 | '\212' -> 778 | '\213' -> 778 | '\214' -> 778 | '\215' -> 584 | '\216' -> 778 | '\217' -> 722 | '\218' -> 722 | '\219' -> 722 | '\220' -> 722 | '\221' -> 667 | '\222' -> 667 | '\223' -> 611 | '\224' -> 556 | '\225' -> 556 | '\226' -> 556 | '\227' -> 556 | '\228' -> 556 | '\229' -> 556 | '\230' -> 889 | '\231' -> 556 | '\232' -> 556 | '\233' -> 556 | '\234' -> 556 | '\235' -> 556 | '\236' -> 278 | '\237' -> 278 | '\238' -> 278 | '\239' -> 278 | '\240' -> 611 | '\241' -> 611 | '\242' -> 611 | '\243' -> 611 | '\244' -> 611 | '\245' -> 611 | '\246' -> 611 | '\247' -> 584 | '\248' -> 611 | '\249' -> 611 | '\250' -> 611 | '\251' -> 611 | '\252' -> 611 | '\253' -> 556 | '\254' -> 611 | '\255' -> 556
end

module Times (*: FONT*) =
struct
  let normal = function '\000'..'\031' -> 250 | ' ' -> 250 | '!' -> 333 | '"' -> 408 | '#' -> 500 | '$' -> 500 | '%' -> 833 | '&' -> 778 | '\'' -> 180 | '(' -> 333 | ')' -> 333 | '*' -> 500 | '+' -> 564 | '-' -> 333 | ',' -> 250 | '.' -> 250 | '/' -> 278 | '0'..'9' -> 500 | ':' -> 278 | ';' -> 278 | '<' -> 564 | '=' -> 564 | '>' -> 564 | '?' -> 444 | '@' -> 921 | 'A' -> 722 | 'B' -> 667 | 'C' -> 667 | 'D' -> 722 | 'E' -> 611 | 'F' -> 556 | 'G' -> 722 | 'H' -> 722 | 'I' -> 333 | 'J' -> 389 | 'K' -> 722 | 'L' -> 611 | 'M' -> 889 | 'N' -> 722 | 'O' -> 722 | 'P' -> 556 | 'Q' -> 722 | 'R' -> 667 | 'S' -> 556 | 'T' -> 611 | 'U' -> 722 | 'V' -> 722 | 'W' -> 944 | 'X' -> 722 | 'Y' -> 722 | 'Z' -> 611 | '[' -> 333 | '\\' -> 278 | ']' -> 333 | '^' -> 469 | '_' -> 500 | '`' -> 333 | 'a' -> 444 | 'b' -> 500 | 'c' -> 444 | 'd' -> 500 | 'e' -> 444 | 'f' -> 333 | 'g' -> 500 | 'h' -> 500 | 'i' -> 278 | 'j' -> 278 | 'k' -> 500 | 'l' -> 278 | 'm' -> 778 | 'n' -> 500 | 'o' -> 500 | 'p' -> 500 | 'q' -> 500 | 'r' -> 333 | 's' -> 389 | 't' -> 278 | 'u' -> 500 | 'v' -> 500 | 'w' -> 722 | 'x' -> 500 | 'y' -> 500 | 'z' -> 444 | '{' -> 480 | '|' -> 200 | '}' -> 480 | '~' -> 541 | '\127' -> 350 | '\128' -> 500 | '\129' -> 350 | '\130' -> 333 | '\131' -> 500 | '\132' -> 444 | '\133' -> 1000 | '\134' -> 500 | '\135' -> 500 | '\136' -> 333 | '\137' -> 1000 | '\138' -> 556 | '\139' -> 333 | '\140' -> 889 | '\141' -> 350 | '\142' -> 611 | '\143' -> 350 | '\144' -> 350 | '\145' -> 333 | '\146' -> 333 | '\147' -> 444 | '\148' -> 444 | '\149' -> 350 | '\150' -> 500 | '\151' -> 1000 | '\152' -> 333 | '\153' -> 980 | '\154' -> 389 | '\155' -> 333 | '\156' -> 722 | '\157' -> 350 | '\158' -> 444 | '\159' -> 722 | '\160' -> 250 | '\161' -> 333 | '\162' -> 500 | '\163' -> 500 | '\164' -> 500 | '\165' -> 500 | '\166' -> 200 | '\167' -> 500 | '\168' -> 333 | '\169' -> 760 | '\170' -> 276 | '\171' -> 500 | '\172' -> 564 | '\173' -> 333 | '\174' -> 760 | '\175' -> 333 | '\176' -> 400 | '\177' -> 564 | '\178' -> 300 | '\179' -> 300 | '\180' -> 333 | '\181' -> 500 | '\182' -> 453 | '\183' -> 250 | '\184' -> 333 | '\185' -> 300 | '\186' -> 310 | '\187' -> 500 | '\188' -> 750 | '\189' -> 750 | '\190' -> 750 | '\191' -> 444 | '\192' -> 722 | '\193' -> 722 | '\194' -> 722 | '\195' -> 722 | '\196' -> 722 | '\197' -> 722 | '\198' -> 889 | '\199' -> 667 | '\200' -> 611 | '\201' -> 611 | '\202' -> 611 | '\203' -> 611 | '\204' -> 333 | '\205' -> 333 | '\206' -> 333 | '\207' -> 333 | '\208' -> 722 | '\209' -> 722 | '\210' -> 722 | '\211' -> 722 | '\212' -> 722 | '\213' -> 722 | '\214' -> 722 | '\215' -> 564 | '\216' -> 722 | '\217' -> 722 | '\218' -> 722 | '\219' -> 722 | '\220' -> 722 | '\221' -> 722 | '\222' -> 556 | '\223' -> 500 | '\224' -> 444 | '\225' -> 444 | '\226' -> 444 | '\227' -> 444 | '\228' -> 444 | '\229' -> 444 | '\230' -> 667 | '\231' -> 444 | '\232' -> 444 | '\233' -> 444 | '\234' -> 444 | '\235' -> 444
    | '\236' -> 278 
    | '\237' -> 278 | '\238' -> 278 | '\239' -> 278 | '\240' -> 500 | '\241' -> 500 | '\242' -> 500 | '\243' -> 500 | '\244' -> 500 | '\245' -> 500 | '\246' -> 500 | '\247' -> 564 | '\248' -> 500 | '\249' -> 500 | '\250' -> 500 | '\251' -> 500 | '\252' -> 500 | '\253' -> 500 | '\254' -> 500 | '\255' -> 500
  let bold = function '\000'..'\031' | ' ' -> 250 | '!' -> 333 | '"' -> 555 | '#' -> 500 | '$' -> 500 | '%' -> 1000 | '&' -> 833 | '\'' -> 278 | '(' -> 333 | ')' -> 333 | '*' -> 500 | '+' -> 570 | ',' -> 250 | '-' -> 333 | '.' -> 250 | '/' -> 278 | '0' -> 500 | '1' -> 500 | '2' -> 500 | '3' -> 500 | '4' -> 500 | '5' -> 500 | '6' -> 500 | '7' -> 500 | '8' -> 500 | '9' -> 500 | ':' -> 333 | ';' -> 333 | '<' -> 570 | '=' -> 570 | '>' -> 570 | '?' -> 500 | '@' -> 930 | 'A' -> 722 | 'B' -> 667 | 'C' -> 722 | 'D' -> 722 | 'E' -> 667 | 'F' -> 611 | 'G' -> 778 | 'H' -> 778 | 'I' -> 389 | 'J' -> 500 | 'K' -> 778 | 'L' -> 667 | 'M' -> 944 | 'N' -> 722 | 'O' -> 778 | 'P' -> 611 | 'Q' -> 778 | 'R' -> 722 | 'S' -> 556 | 'T' -> 667 | 'U' -> 722 | 'V' -> 722 | 'W' -> 1000 | 'X' -> 722 | 'Y' -> 722 | 'Z' -> 667 | '[' -> 333 | '\\' -> 278 | ']' -> 333 | '^' -> 581 | '_' -> 500 | '`' -> 333 | 'a' -> 500 | 'b' -> 556 | 'c' -> 444 | 'd' -> 556 | 'e' -> 444 | 'f' -> 333 | 'g' -> 500 | 'h' -> 556 | 'i' -> 278 | 'j' -> 333 | 'k' -> 556 | 'l' -> 278 | 'm' -> 833 | 'n' -> 556 | 'o' -> 500 | 'p' -> 556 | 'q' -> 556 | 'r' -> 444 | 's' -> 389 | 't' -> 333 | 'u' -> 556 | 'v' -> 500 | 'w' -> 722 | 'x' -> 500 | 'y' -> 500 | 'z' -> 444 | '{' -> 394 | '|' -> 220 | '}' -> 394 | '~' -> 520 | '\127' -> 350 | '\128' -> 500 | '\129' -> 350 | '\130' -> 333 | '\131' -> 500 | '\132' -> 500 | '\133' -> 1000 | '\134' -> 500 | '\135' -> 500 | '\136' -> 333 | '\137' -> 1000 | '\138' -> 556 | '\139' -> 333 | '\140' -> 1000 | '\141' -> 350 | '\142' -> 667 | '\143' -> 350 | '\144' -> 350 | '\145' -> 333 | '\146' -> 333 | '\147' -> 500 | '\148' -> 500 | '\149' -> 350 | '\150' -> 500 | '\151' -> 1000 | '\152' -> 333 | '\153' -> 1000 | '\154' -> 389 | '\155' -> 333 | '\156' -> 722 | '\157' -> 350 | '\158' -> 444 | '\159' -> 722 | '\160' -> 250 | '\161' -> 333 | '\162' -> 500 | '\163' -> 500 | '\164' -> 500 | '\165' -> 500 | '\166' -> 220 | '\167' -> 500 | '\168' -> 333 | '\169' -> 747 | '\170' -> 300 | '\171' -> 500 | '\172' -> 570 | '\173' -> 333 | '\174' -> 747 | '\175' -> 333 | '\176' -> 400 | '\177' -> 570 | '\178' -> 300 | '\179' -> 300 | '\180' -> 333 | '\181' -> 556 | '\182' -> 540 | '\183' -> 250 | '\184' -> 333 | '\185' -> 300 | '\186' -> 330 | '\187' -> 500 | '\188' -> 750 | '\189' -> 750 | '\190' -> 750 | '\191' -> 500 | '\192' -> 722 | '\193' -> 722 | '\194' -> 722 | '\195' -> 722 | '\196' -> 722 | '\197' -> 722 | '\198' -> 1000 | '\199' -> 722 | '\200' -> 667 | '\201' -> 667 | '\202' -> 667 | '\203' -> 667 | '\204' -> 389 | '\205' -> 389 | '\206' -> 389 | '\207' -> 389 | '\208' -> 722 | '\209' -> 722 | '\210' -> 778 | '\211' -> 778 | '\212' -> 778 | '\213' -> 778 | '\214' -> 778 | '\215' -> 570 | '\216' -> 778 | '\217' -> 722 | '\218' -> 722 | '\219' -> 722 | '\220' -> 722 | '\221' -> 722 | '\222' -> 611 | '\223' -> 556 | '\224' -> 500 | '\225' -> 500 | '\226' -> 500 | '\227' -> 500 | '\228' -> 500 | '\229' -> 500 | '\230' -> 722 | '\231' -> 444 | '\232' -> 444 | '\233' -> 444 | '\234' -> 444 | '\235' -> 444 | '\236' -> 278 | '\237' -> 278 | '\238' -> 278 | '\239' -> 278 | '\240' -> 500 | '\241' -> 556 | '\242' -> 500 | '\243' -> 500 | '\244' -> 500 | '\245' -> 500 | '\246' -> 500 | '\247' -> 570 | '\248' -> 500 | '\249' -> 556 | '\250' -> 556 | '\251' -> 556 | '\252' -> 556 | '\253' -> 500 | '\254' -> 556 | '\255' -> 500
  let italic = function '\000'..'\031' -> 250 | ' ' -> 250 | '!' -> 333 | '"' -> 420 | '#' -> 500 | '$' -> 500 | '%' -> 833 | '&' -> 778 | '\'' -> 214 | '(' -> 333 | ')' -> 333 | '*' -> 500 | '+' -> 675 | ',' -> 250 | '-' -> 333 | '.' -> 250 | '/' -> 278 | '0'..'9' -> 500 | ':' -> 333 | ';' -> 333 | '<' -> 675 | '=' -> 675 | '>' -> 675 | '?' -> 500 | '@' -> 920 | 'A' -> 611 | 'B' -> 611 | 'C' -> 667 | 'D' -> 722 | 'E' -> 611 | 'F' -> 611 | 'G' -> 722 | 'H' -> 722 | 'I' -> 333 | 'J' -> 444 | 'K' -> 667 | 'L' -> 556 | 'M' -> 833 | 'N' -> 667 | 'O' -> 722 | 'P' -> 611 | 'Q' -> 722 | 'R' -> 611 | 'S' -> 500 | 'T' -> 556 | 'U' -> 722 | 'V' -> 611 | 'W' -> 833 | 'X' -> 611 | 'Y' -> 556 | 'Z' -> 556 | '[' -> 389 | '\\' ->278 | ']' -> 389 | '^' -> 422 | '_' -> 500 | '`' -> 333 | 'a' -> 500 | 'b' -> 500 | 'c' -> 444 | 'd' -> 500 | 'e' -> 444 | 'f' -> 278 | 'g' -> 500 | 'h' -> 500 | 'i' -> 278 | 'j' -> 278 | 'k' -> 444 | 'l' -> 278 | 'm' -> 722 | 'n' -> 500 | 'o' -> 500 | 'p' -> 500 | 'q' -> 500 | 'r' -> 389 | 's' -> 389 | 't' -> 278 | 'u' -> 500 | 'v' -> 444 | 'w' -> 667 | 'x' -> 444 | 'y' -> 444 | 'z' -> 389 | '{' -> 400 | '|' -> 275 | '}' -> 400 | '~' -> 541 | '\127' -> 350 | '\128' -> 500 | '\129' -> 350 | '\130' -> 333 | '\131' -> 500 | '\132' -> 556 | '\133' -> 889 | '\134' -> 500 | '\135' -> 500 | '\136' -> 333 | '\137' -> 1000 | '\138' -> 500 | '\139' -> 333 | '\140' -> 944 | '\141' -> 350 | '\142' -> 556 | '\143' -> 350 | '\144' -> 350 | '\145' -> 333 | '\146' -> 333 | '\147' -> 556 | '\148' -> 556 | '\149' -> 350 | '\150' -> 500 | '\151' -> 889 | '\152' -> 333 | '\153' -> 980 | '\154' -> 389 | '\155' -> 333 | '\156' -> 667 | '\157' -> 350 | '\158' -> 389 | '\159' -> 556 | '\160' -> 250 | '\161' -> 389 | '\162' -> 500 | '\163' -> 500 | '\164' -> 500 | '\165' -> 500 | '\166' -> 275 | '\167' -> 500 | '\168' -> 333 | '\169' -> 760 | '\170' -> 276 | '\171' -> 500 | '\172' -> 675 | '\173' -> 333 | '\174' -> 760 | '\175' -> 333 | '\176' -> 400 | '\177' -> 675 | '\178' -> 300 | '\179' -> 300 | '\180' -> 333 | '\181' -> 500 | '\182' -> 523 | '\183' -> 250 | '\184' -> 333 | '\185' -> 300 | '\186' -> 310 | '\187' -> 500 | '\188' -> 750 | '\189' -> 750 | '\190' -> 750 | '\191' -> 500 | '\192' -> 611 | '\193' -> 611 | '\194' -> 611 | '\195' -> 611 | '\196' -> 611 | '\197' -> 611 | '\198' -> 889 | '\199' -> 667 | '\200' -> 611 | '\201' -> 611 | '\202' -> 611 | '\203' -> 611 | '\204' -> 333 | '\205' -> 333 | '\206' -> 333 | '\207' -> 333 | '\208' -> 722 | '\209' -> 667 | '\210' -> 722 | '\211' -> 722 | '\212' -> 722 | '\213' -> 722 | '\214' -> 722 | '\215' -> 675 | '\216' -> 722 | '\217' -> 722 | '\218' -> 722 | '\219' -> 722 | '\220' -> 722 | '\221' -> 556 | '\222' -> 611 | '\223' -> 500 | '\224' -> 500 | '\225' -> 500 | '\226' -> 500 | '\227' -> 500 | '\228' -> 500 | '\229' -> 500 | '\230' -> 667 | '\231' -> 444 | '\232' -> 444 | '\233' -> 444 | '\234' -> 444 | '\235' -> 444 | '\236' -> 278 | '\237' -> 278 | '\238' -> 278 | '\239' -> 278 | '\240' -> 500 | '\241' -> 500 | '\242' -> 500 | '\243' -> 500 | '\244' -> 500 | '\245' -> 500 | '\246' -> 500 | '\247' -> 675 | '\248' -> 500 | '\249' -> 500 | '\250' -> 500 | '\251' -> 500 | '\252' -> 500 | '\253' -> 444 | '\254' -> 500 | '\255' -> 444
  let bold_italic = function  '\000'..'\031' -> 250 | ' ' -> 250 | '!' -> 389 | '"' -> 555 | '#' -> 500 | '$' -> 500 | '%' -> 833 | '&' -> 778 | '\'' -> 278 | '(' -> 333 | ')' -> 333 | '*' -> 500 | '+' -> 570 | ',' -> 250 | '-' -> 333 | '.' -> 250 | '/' -> 278 | '0' -> 500 | '1' -> 500 | '2' -> 500 | '3' -> 500 | '4' -> 500 | '5' -> 500 | '6' -> 500 | '7' -> 500 | '8' -> 500 | '9' -> 500 | ':' -> 333 | ';' -> 333 | '<' -> 570 | '=' -> 570 | '>' -> 570 | '?' -> 500 | '@' -> 832 | 'A' -> 667 | 'B' -> 667 | 'C' -> 667 | 'D' -> 722 | 'E' -> 667 | 'F' -> 667 | 'G' -> 722 | 'H' -> 778 | 'I' -> 389 | 'J' -> 500 | 'K' -> 667 | 'L' -> 611 | 'M' -> 889 | 'N' -> 722 | 'O' -> 722 | 'P' -> 611 | 'Q' -> 722 | 'R' -> 667 | 'S' -> 556 | 'T' -> 611 | 'U' -> 722 | 'V' -> 667 | 'W' -> 889 | 'X' -> 667 | 'Y' -> 611 | 'Z' -> 611 | '[' -> 333 | '\\' -> 278 | ']' -> 333 | '^' -> 570 | '_' -> 500 | '`' -> 333 | 'a' -> 500 | 'b' -> 500 | 'c' -> 444 | 'd' -> 500 | 'e' -> 444 | 'f' -> 333 | 'g' -> 500 | 'h' -> 556 | 'i' -> 278 | 'j' -> 278 | 'k' -> 500 | 'l' -> 278 | 'm' -> 778 | 'n' -> 556 | 'o' -> 500 | 'p' -> 500 | 'q' -> 500 | 'r' -> 389 | 's' -> 389 | 't' -> 278 | 'u' -> 556 | 'v' -> 444 | 'w' -> 667 | 'x' -> 500 | 'y' -> 444 | 'z' -> 389 | '{' -> 348 | '|' -> 220 | '}' -> 348 | '~' -> 570 | '\127' -> 350 | '\128' -> 500 | '\129' -> 350 | '\130' -> 333 | '\131' -> 500 | '\132' -> 500 | '\133' -> 1000 | '\134' -> 500 | '\135' -> 500 | '\136' -> 333 | '\137' -> 1000 | '\138' -> 556 | '\139' -> 333 | '\140' -> 944 | '\141' -> 350 | '\142' -> 611 | '\143' -> 350 | '\144' -> 350 | '\145' -> 333 | '\146' -> 333 | '\147' -> 500 | '\148' -> 500 | '\149' -> 350 | '\150' -> 500 | '\151' -> 1000 | '\152' -> 333 | '\153' -> 1000 | '\154' -> 389 | '\155' -> 333 | '\156' -> 722 | '\157' -> 350 | '\158' -> 389 | '\159' -> 611 | '\160' -> 250 | '\161' -> 389 | '\162' -> 500 | '\163' -> 500 | '\164' -> 500 | '\165' -> 500 | '\166' -> 220 | '\167' -> 500 | '\168' -> 333 | '\169' -> 747 | '\170' -> 266 | '\171' -> 500 | '\172' -> 606 | '\173' -> 333 | '\174' -> 747 | '\175' -> 333 | '\176' -> 400 | '\177' -> 570 | '\178' -> 300 | '\179' -> 300 | '\180' -> 333 | '\181' -> 576 | '\182' -> 500 | '\183' -> 250 | '\184' -> 333 | '\185' -> 300 | '\186' -> 300 | '\187' -> 500 | '\188' -> 750 | '\189' -> 750 | '\190' -> 750 | '\191' -> 500 | '\192' -> 667 | '\193' -> 667 | '\194' -> 667 | '\195' -> 667 | '\196' -> 667 | '\197' -> 667 | '\198' -> 944 | '\199' -> 667 | '\200' -> 667 | '\201' -> 667 | '\202' -> 667 | '\203' -> 667 | '\204' -> 389 | '\205' -> 389 | '\206' -> 389 | '\207' -> 389 | '\208' -> 722 | '\209' -> 722 | '\210' -> 722 | '\211' -> 722 | '\212' -> 722 | '\213' -> 722 | '\214' -> 722 | '\215' -> 570 | '\216' -> 722 | '\217' -> 722 | '\218' -> 722 | '\219' -> 722 | '\220' -> 722 | '\221' -> 611 | '\222' -> 611 | '\223' -> 500 | '\224' -> 500 | '\225' -> 500 | '\226' -> 500 | '\227' -> 500 | '\228' -> 500 | '\229' -> 500 | '\230' -> 722 | '\231' -> 444 | '\232' -> 444 | '\233' -> 444 | '\234' -> 444 | '\235' -> 444 | '\236' -> 278 | '\237' -> 278 | '\238' -> 278 | '\239' -> 278 | '\240' -> 500 | '\241' -> 556 | '\242' -> 500 | '\243' -> 500 | '\244' -> 500 | '\245' -> 500 | '\246' -> 500 | '\247' -> 570 | '\248' -> 500 | '\249' -> 556 | '\250' -> 556 | '\251' -> 556 | '\252' -> 556 | '\253' -> 444 | '\254' -> 500 | '\255' -> 444
end

