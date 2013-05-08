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
  fontName           = "ZapfDingbats";
  fontBBox           = -1, -143, 981, 820; (* lowerLeftX, lowerLeftY, upperRightX, upperRightY *)
  underlinePosition  = -100;
  underlineThickness = 50;
  charMetrics        = function
    | '\000'..'\031' -> 0 | ' ' -> 278 | '!' -> 974 | '"' -> 961 | '#' -> 974
    | '$' -> 980 | '%' -> 719 | '&' -> 789 | '\'' -> 790 | '(' -> 791 | ')' -> 690
    | '*' -> 960 | '+' -> 939 | ',' -> 549 | '-' -> 855 | '.' -> 911  | '/' -> 933
    | '0' -> 911 | '1' -> 945 | '2' -> 974 | '3' -> 755 | '4' -> 846 | '5' -> 762
    | '6' -> 761 | '7' -> 571 | '8' -> 677 | '9' -> 763 | ':' -> 760 | ';' -> 759
    | '<' -> 754 | '=' -> 494 | '>' -> 552 | '?' -> 537 | '@' -> 577 | 'A' -> 692
    | 'B' -> 786 | 'C' -> 788 | 'D' -> 788 | 'E' -> 790 | 'F' -> 793 | 'G' -> 794
    | 'H' -> 816 | 'I' -> 823 | 'J' -> 789 | 'K' -> 841 | 'L' -> 823 | 'M' -> 833
    | 'N' -> 816 | 'O' -> 831 | 'P' -> 923 | 'Q' -> 744 | 'R' -> 723 | 'S' -> 749
    | 'T' -> 790 | 'U' -> 792 | 'V' -> 695 | 'W' -> 776 | 'X' -> 768 | 'Y' -> 792
    | 'Z' -> 759 | '[' -> 707 | '\\' -> 708 | ']' -> 682 | '^' -> 701 | '_' -> 826
    | '`' -> 815 | 'a' -> 789 | 'b' -> 789 | 'c' -> 707 | 'd' -> 687 | 'e' -> 696
    | 'f' -> 689 | 'g' -> 786 | 'h' -> 787 | 'i' -> 713 | 'j' -> 791 | 'k' -> 785
    | 'l' -> 791 | 'm' -> 873 |'n' -> 761 | 'o' -> 762 | 'p' -> 762 | 'q' -> 759
    | 'r' -> 759 | 's' -> 892 | 't' -> 892 | 'u' -> 788 | 'v' -> 784 | 'w' -> 438
    | 'x' -> 138 | 'y' -> 277 | 'z' -> 415 | '{' -> 392 | '|' -> 392 | '}' -> 668
    | '~' -> 668 | '\127' -> 0 | '\128' -> 390 | '\129' -> 390 | '\130' -> 317
    | '\131' -> 317 | '\132' -> 276 | '\133' -> 276 | '\134' -> 509 | '\135' -> 509
    | '\136' -> 410 | '\137' -> 410 | '\138' -> 234 | '\139' -> 234 | '\140' -> 334
    | '\141' -> 334 | '\142' -> 0 | '\143' -> 0 | '\144' -> 0 | '\145' -> 0
    | '\146' -> 0 | '\147' -> 0 | '\148' -> 0 | '\149' -> 0 | '\150' -> 0
    | '\151' -> 0 | '\152' -> 0 | '\153' -> 0 | '\154' -> 0 | '\155' -> 0
    | '\156' -> 0 | '\157' -> 0 | '\158' -> 0 | '\159' -> 0 | '\160' -> 0
    | '\161' -> 732 | '\162' -> 544 | '\163' -> 544 | '\164' -> 910 | '\165' -> 667
    | '\166' -> 760 | '\167' -> 760 | '\168' -> 776 | '\169' -> 595 | '\170' -> 694
    | '\171' -> 626 | '\172' -> 788 | '\173' -> 788 | '\174' -> 788 | '\175' -> 788
    | '\176' -> 788 | '\177' -> 788 | '\178' -> 788 | '\179' -> 788 | '\180' -> 788
    | '\181' -> 788 | '\182' -> 788 | '\183' -> 788 | '\184' -> 788 | '\185' -> 788
    | '\186' -> 788 | '\187' -> 788 | '\188' -> 788 | '\189' -> 788 | '\190' -> 788
    | '\191' -> 788 | '\192' -> 788 | '\193' -> 788 | '\194' -> 788 | '\195' -> 788
    | '\196' -> 788 | '\197' -> 788 | '\198' -> 788 | '\199' -> 788 | '\200' -> 788
    | '\201' -> 788 | '\202' -> 788 | '\203' -> 788 | '\204' -> 788 | '\205' -> 788
    | '\206' -> 788 | '\207' -> 788 | '\208' -> 788 | '\209' -> 788 | '\210' -> 788
    | '\211' -> 788 | '\212' -> 894 | '\213' -> 838 | '\214' -> 1016 | '\215' -> 458
    | '\216' -> 748 | '\217' -> 924 | '\218' -> 748 | '\219' -> 918 | '\220' -> 927
    | '\221' -> 928 | '\222' -> 928 | '\223' -> 834 | '\224' -> 873 | '\225' -> 828
    | '\226' -> 924 | '\227' -> 924 | '\228' -> 917 | '\229' -> 930 | '\230' -> 931
    | '\231' -> 463 | '\232' -> 883 | '\233' -> 836 | '\234' -> 836 | '\235' -> 867
    | '\236' -> 867 | '\237' -> 696 | '\238' -> 696 | '\239' -> 874 | '\240' -> 0
    | '\241' -> 874 | '\242' -> 760 | '\243' -> 946 | '\244' -> 771 | '\245' -> 865
    | '\246' -> 771 | '\247' -> 888 | '\248' -> 967 | '\249' -> 888 | '\250' -> 831
    | '\251' -> 873 | '\252' -> 927 | '\253' -> 970 | '\254' -> 918 | '\255' -> 0
}
