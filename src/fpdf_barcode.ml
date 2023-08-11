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

open Printf

module Code39 =
struct
  let bar_char = [
    ('0', "nnnwwnwnn");
    ('1', "wnnwnnnnw");
    ('2', "nnwwnnnnw");
    ('3', "wnwwnnnnn");
    ('4', "nnnwwnnnw");
    ('5', "wnnwwnnnn");
    ('6', "nnwwwnnnn");
    ('7', "nnnwnnwnw");
    ('8', "wnnwnnwnn");
    ('9', "nnwwnnwnn");
    ('A', "wnnnnwnnw");
    ('B', "nnwnnwnnw");
    ('C', "wnwnnwnnn");
    ('D', "nnnnwwnnw");
    ('E', "wnnnwwnnn");
    ('F', "nnwnwwnnn");
    ('G', "nnnnnwwnw");
    ('H', "wnnnnwwnn");
    ('I', "nnwnnwwnn");
    ('J', "nnnnwwwnn");
    ('K', "wnnnnnnww");
    ('L', "nnwnnnnww");
    ('M', "wnwnnnnwn");
    ('N', "nnnnwnnww");
    ('O', "wnnnwnnwn");
    ('P', "nnwnwnnwn");
    ('Q', "nnnnnnwww");
    ('R', "wnnnnnwwn");
    ('S', "nnwnnnwwn");
    ('T', "nnnnwnwwn");
    ('U', "wwnnnnnnw");
    ('V', "nwwnnnnnw");
    ('W', "wwwnnnnnn");
    ('X', "nwnnwnnnw");
    ('Y', "wwnnwnnnn");
    ('Z', "nwwnwnnnn");
    ('-', "nwnnnnwnw");
    ('.', "wwnnnnwnn");
    (' ', "nwwnnnwnn");
    ('*', "nwnnwnwnn");
    ('$', "nwnwnwnnn");
    ('/', "nwnwnnnwn");
    ('+', "nwnnnwnwn");
    ('%', "nnnwnwnwn");
  ]

  let get_width ~barcode = (float (String.length barcode + 2)) *. 16. /. 3. +. 2.

  let write ~x ~y ~barcode ?(baseline=0.5) ?(height=5.0) ?text doc =
    let wide = baseline in
    let narrow = baseline /. 3. in
    let gap = narrow in
    let input_barcode = barcode in
    let barcode = Printf.sprintf "*%s*" (String.uppercase_ascii barcode) in
    let margin = baseline *. 1. in (* margine sx *)
    begin
      match text with None -> () | Some size ->
        let barcode_width = baseline *. (get_width ~barcode:input_barcode)  in
        Fpdf.set_font ~size doc;
        let font =
          match doc.Fpdf_document.current_font with
            | Some font -> font.Fpdf_document.font_metrics
            | _ -> failwith "No current font"
        in
        let text_width = Fpdf.get_text_width font size None barcode /. Fpdf.scale doc in
        let x = x +. margin +. (barcode_width -. text_width) /. 2. in
        let y = y +. height +. (size -. 1.) /. Fpdf.scale doc in
        Fpdf.text ~x ~y ~text:barcode doc;
    end;
    let x = ref (x +. margin) in
    String.iter begin fun ch ->
      try
        let seq = List.assoc ch bar_char in
        for i = 0 to 8 do
          let line_width = if seq.[i] = 'n' then narrow else wide in
          if i mod 2 = 0 then begin
            Fpdf.rect ~x:!x ~y ~width:line_width ~height ~style:`Fill doc
          end;
          x := !x +. line_width
        done;
        x := !x +. gap
      with Not_found -> failwith ("Invalid character in barcode: " ^ (String.make 1 ch))
    end barcode
end

module EAN13 =
struct

  exception Incorrect_check_digit of string

  let lpad str len ch =
    let len' = String.length str in
    if len' > len then str (*String.sub str 0 len*)
    else
      let result = Bytes.make len ch in
      String.blit str 0 result (len - len') len';
      result |> Bytes.to_string

  (** Convert digits to bars *)
  let codes = function
    | 'A' -> begin function
        | '0' -> "0001101" | '1' -> "0011001" | '2' -> "0010011"
        | '3' -> "0111101" | '4' -> "0100011" | '5' -> "0110001"
        | '6' -> "0101111" | '7' -> "0111011" | '8' -> "0110111"
        | '9' -> "0001011" | _ -> assert false end
    | 'B' -> begin function
        | '0' -> "0100111" | '1' -> "0110011" | '2' -> "0011011"
        | '3' -> "0100001" | '4' -> "0011101" | '5' -> "0111001"
        | '6' -> "0000101" | '7' -> "0010001" | '8' -> "0001001"
        | '9' -> "0010111" | _ -> assert false end
    | 'C' -> begin function
        | '0' -> "1110010" | '1' -> "1100110" | '2' -> "1101100"
        | '3' -> "1000010" | '4' -> "1011100" | '5' -> "1001110"
        | '6' -> "1010000" | '7' -> "1000100" | '8' -> "1001000"
        | '9' -> "1110100" | _ -> assert false end
    | _ -> assert false

  let parities = function
    | '0' -> [| 'A'; 'A'; 'A'; 'A'; 'A'; 'A' |]
    | '1' -> [| 'A'; 'A'; 'B'; 'A'; 'B'; 'B' |]
    | '2' -> [| 'A'; 'A'; 'B'; 'B'; 'A'; 'B' |]
    | '3' -> [| 'A'; 'A'; 'B'; 'B'; 'B'; 'A' |]
    | '4' -> [| 'A'; 'B'; 'A'; 'A'; 'B'; 'B' |]
    | '5' -> [| 'A'; 'B'; 'B'; 'A'; 'A'; 'B' |]
    | '6' -> [| 'A'; 'B'; 'B'; 'B'; 'A'; 'A' |]
    | '7' -> [| 'A'; 'B'; 'A'; 'B'; 'A'; 'B' |]
    | '8' -> [| 'A'; 'B'; 'A'; 'B'; 'B'; 'A' |]
    | '9' -> [| 'A'; 'B'; 'B'; 'A'; 'B'; 'A' |]
    | _ -> assert false

  let int_of_digit x = int_of_string (String.make 1 x)

  let test_check_digit barcode =
    (* Test validity of check digit *)
    let sum = List.fold_left (fun acc i -> 3 * (int_of_digit barcode.[i]) + acc) 0
        [1; 3; 5; 7; 9; 11] in
    let sum = List.fold_left (fun acc i -> (int_of_digit barcode.[i]) + acc) sum
        [0; 2; 4; 6; 8; 10] in
    (sum + (int_of_digit barcode.[12])) mod 10 = 0

  let get_check_digit barcode =
    (* Compute the check digit *)
    let sum = List.fold_left (fun acc i -> 3 * (int_of_digit barcode.[i]) + acc) 0
        [1; 3; 5; 7; 9; 11] in
    let sum = List.fold_left (fun acc i -> (int_of_digit barcode.[i]) + acc) sum
        [0; 2; 4; 6; 8; 10] in
    let result = sum mod 10 in
    string_of_int (if result > 0 then 10 - result else result)

  let rec fixpoint f v =
    let v' = f v in
    if v = v' then v else fixpoint f v'

  let write ~x ~y ?(height=16.) ?(width=0.35) ~barcode ?(upc_a=false) doc =
    (*    if upc_a && String.length barcode > 12 then invalid_arg (Printf.sprintf "Barcode too long (%s)" barcode)
          else if not *)
    let len = if upc_a then 12 else 13 in
    let input_barcode = barcode in
    (* Padding: bisogna troncare o no? *)
    let barcode = lpad barcode (len - 1) '0' in
    let barcode = if len = 12 then "0" ^ barcode else barcode in
    (* Add or control the check digit *)
    let barcode = if String.length barcode = 12 then barcode ^ (get_check_digit barcode)
      else if not (test_check_digit barcode) then (raise (Incorrect_check_digit input_barcode))
      else barcode in
    let code = ref "101" in
    let p = parities barcode.[0] in
    for i = 1 to 6 do
      code := !code ^ codes p.(i - 1) barcode.[i]
    done;
    code := !code ^ "01010";
    for i = 7 to 12 do
      code := !code ^ codes 'C' barcode.[i];
    done;
    code := !code ^ "101";
    (* Draw bars *)
    let k = Fpdf.scale doc in
    let family = Fpdf.font_family doc in
    let size = Fpdf.font_size doc in
    let style = Fpdf.font_style doc in
    let len = String.length !code in
    let font =
      let family = `Helvetica in
      let style = [] in
      let f () = Fpdf_document.find_font ~family ~style doc in
      match f () with Some x -> x | _ ->
        let old_family = Fpdf.font_family doc in
        let old_style = Fpdf.font_style doc in
        Fpdf.set_font ~family ~style doc;
        let x = f () in
        Fpdf.set_font ?family:old_family ~style:old_style doc;
        match x with Some a -> a | _ -> assert false
    in
    let width' = 60. *. width in
    let scale = Fpdf.scale doc in
    let fsize = fixpoint begin fun size ->
        let text_width = Fpdf_text.get_text_width font.Fpdf_document.font_metrics size None barcode in
        if text_width /. scale < width' then size else (size -. 0.25)
      end 30. in
    Fpdf.set_font ~family:`Helvetica ~style:[] ~size:fsize doc;
    let height_ext = height +. 5. *. width in
    let j = ref 0 in
    let x0 = x in
    let print_text i =
      let x = x0 +. (float (i + 1)) *. width in
      Fpdf.text ~x ~y:(y +. height +. fsize /. k) ~text:(String.make 1 barcode.[!j]) doc;
      incr j
    in
    let x = ref 0.0 in
    print_text (-10);
    for i = 0 to String.length !code - 1 do
      if !code.[i] = '1' then begin
        x := x0 +. (float i) *. width;
        let height = if i <= 3 || (45 <= i && i <= 49) || i >= (len - 3) then height_ext else height in
        Fpdf.rect ~x:!x ~y ~width ~height ~style:`Fill doc;
      end;
      match i with
        | 3 -> print_text i
        | 10 -> print_text i
        | 17 -> print_text i
        | 24 -> print_text i
        | 31 -> print_text i
        | 38 -> print_text i
        | 50 -> print_text i
        | 57 -> print_text i
        | 64 -> print_text i
        | 71 -> print_text i
        | 78 -> print_text i
        | 85 -> print_text i
        | _ -> ()
    done;
    Fpdf.set_font ?family ~style ~size doc;
end


module Code128C =
struct

  let encode = function
    | 0 -> "212222"
    | 1 -> "222122"
    | 2 -> "222221"
    | 3 -> "121223"
    | 4 -> "121322"
    | 5 -> "131222"
    | 6 -> "122213"
    | 7 -> "122312"
    | 8 -> "132212"
    | 9 -> "221213"
    | 10 -> "221312"
    | 11 -> "231212"
    | 12 -> "112232"
    | 13 -> "122132"
    | 14 -> "122231"
    | 15 -> "113222"
    | 16 -> "123122"
    | 17 -> "123221"
    | 18 -> "223211"
    | 19 -> "221132"
    | 20 -> "221231"
    | 21 -> "213212"
    | 22 -> "223112"
    | 23 -> "312131"
    | 24 -> "311222"
    | 25 -> "321122"
    | 26 -> "321221"
    | 27 -> "312212"
    | 28 -> "322112"
    | 29 -> "322211"
    | 30 -> "212123"
    | 31 -> "212321"
    | 32 -> "232121"
    | 33 -> "111323"
    | 34 -> "131123"
    | 35 -> "131321"
    | 36 -> "112313"
    | 37 -> "132113"
    | 38 -> "132311"
    | 39 -> "211313"
    | 40 -> "231113"
    | 41 -> "231311"
    | 42 -> "112133"
    | 43 -> "112331"
    | 44 -> "132131"
    | 45 -> "113123"
    | 46 -> "113321"
    | 47 -> "133121"
    | 48 -> "313121"
    | 49 -> "211331"
    | 50 -> "231131"
    | 51 -> "213113"
    | 52 -> "213311"
    | 53 -> "213131"
    | 54 -> "311123"
    | 55 -> "311321"
    | 56 -> "331121"
    | 57 -> "312113"
    | 58 -> "312311"
    | 59 -> "332111"
    | 60 -> "314111"
    | 61 -> "221411"
    | 62 -> "431111"
    | 63 -> "111224"
    | 64 -> "111422"
    | 65 -> "121124"
    | 66 -> "121421"
    | 67 -> "141122"
    | 68 -> "141221"
    | 69 -> "112214"
    | 70 -> "112412"
    | 71 -> "122114"
    | 72 -> "122411"
    | 73 -> "142112"
    | 74 -> "142211"
    | 75 -> "241211"
    | 76 -> "221114"
    | 77 -> "413111"
    | 78 -> "241112"
    | 79 -> "134111"
    | 80 -> "111242"
    | 81 -> "121142"
    | 82 -> "121241"
    | 83 -> "114212"
    | 84 -> "124112"
    | 85 -> "124211"
    | 86 -> "411212"
    | 87 -> "421112"
    | 88 -> "421211"
    | 89 -> "212141"
    | 90 -> "214121"
    | 91 -> "412121"
    | 92 -> "111143"
    | 93 -> "111341"
    | 94 -> "131141"
    | 95 -> "114113"
    | 96 -> "114311"
    | 97 -> "411113"
    | 98 -> "411311"
    | 99 -> "113141"
    | 100 -> "114131" (* CODE B *)
    | 101 -> "311141" (* CODE A *)
    | 102 -> "411131" (* FNC1 *)
    | 103 -> "211412" (* START A *)
    | 104 -> "211214" (* START B *)
    | 105 -> "211232" (* START C *)
    | 106 -> "2331112" (* STOP *)
    | _ -> invalid_arg "encode"

  let check_symbol values =
    let values = List.rev values in
    let sum = List.hd values in
    let i = ref 0 in
    let sum = List.fold_left (fun acc v -> incr i; acc + v * !i) sum (List.tl values) in
    sum mod 103

  let get_width ~barcode = (String.length barcode / 2 + 4) * 11 + 13

  let write ~x ~y ?(height=16.) ?(width=0.35) ?(set=(`C : [`A | `B | `C])) ?text ~barcode doc =
    match set with `A | `B -> invalid_arg "write"
                 | `C when String.length barcode mod 2 <> 0 ->
                   invalid_arg (sprintf "Fpdf_barcode.Code128-C: barcode length must be even (%s)." barcode)
                 | `C ->
                   let value_of_c str =
                     if String.length str <> 2 then invalid_arg "value_of_c" else
                       (match int_of_string_opt str with Some x -> x | _ -> invalid_arg "Fpdf_barcode.Code128-C: accepts only numeric data.")
                   in
                   let i = ref 0 in
                   let len = String.length barcode in
                   let values = ref [105] in (* 105 = START C *)
                   while !i < len do
                     values := (value_of_c (String.sub barcode !i 2)) :: !values;
                     i := !i + 2;
                   done;
                   values := 106 :: (check_symbol !values) :: !values; (* 106 = STOP *)
                   let code = String.concat "" (List.rev_map encode !values) in
                   (* Draw bars *)
                   let baseline = width in
                   let margin = baseline *. 11. in
                   let pos = ref (x +. margin) in
                   for i = 0 to String.length code - 1 do
                     let width = baseline *. (float_of_string (String.sub code i 1)) in
                     if i mod 2 = 0 then (Fpdf.rect ~x:!pos ~y ~width ~height ~style:`Fill doc);
                     pos := !pos +. width
                   done;
                   match text with None -> () | Some size ->
                     Fpdf.set_font ~size doc;
                     let text = Printf.sprintf "%s" barcode (*(check_symbol !values)*) in
                     let font =
                       match doc.Fpdf_document.current_font with
                         | Some font -> font.Fpdf_document.font_metrics
                         | _ -> failwith "No current font"
                     in
                     let text_width = Fpdf.get_text_width font size None text /. Fpdf.scale doc in
                     let barcode_width = float (get_width ~barcode) *. baseline in
                     let x = x +. (barcode_width -. text_width) /. 2. in
                     Fpdf.text ~x ~y:(y +. height +. (size -. 1.) /. Fpdf.scale doc) ~text doc
end



(*module Code128 =
  struct

    let encode = function
      | 0 -> "212222"
      | 1 -> "222122"
      | 2 -> "222221"
      | 3 -> "121223"
      | 4 -> "121322"
      | 5 -> "131222"
      | 6 -> "122213"
      | 7 -> "122312"
      | 8 -> "132212"
      | 9 -> "221213"
      | 10 -> "221312"
      | 11 -> "231212"
      | 12 -> "112232"
      | 13 -> "122132"
      | 14 -> "122231"
      | 15 -> "113222"
      | 16 -> "123122"
      | 17 -> "123221"
      | 18 -> "223211"
      | 19 -> "221132"
      | 20 -> "221231"
      | 21 -> "213212"
      | 22 -> "223112"
      | 23 -> "312131"
      | 24 -> "311222"
      | 25 -> "321122"
      | 26 -> "321221"
      | 27 -> "312212"
      | 28 -> "322112"
      | 29 -> "322211"
      | 30 -> "212123"
      | 31 -> "212321"
      | 32 -> "232121"
      | 33 -> "111323"
      | 34 -> "131123"
      | 35 -> "131321"
      | 36 -> "112313"
      | 37 -> "132113"
      | 38 -> "132311"
      | 39 -> "211313"
      | 40 -> "231113"
      | 41 -> "231311"
      | 42 -> "112133"
      | 43 -> "112331"
      | 44 -> "132131"
      | 45 -> "113123"
      | 46 -> "113321"
      | 47 -> "133121"
      | 48 -> "313121"
      | 49 -> "211331"
      | 50 -> "231131"
      | 51 -> "213113"
      | 52 -> "213311"
      | 53 -> "213131"
      | 54 -> "311123"
      | 55 -> "311321"
      | 56 -> "331121"
      | 57 -> "312113"
      | 58 -> "312311"
      | 59 -> "332111"
      | 60 -> "314111"
      | 61 -> "221411"
      | 62 -> "431111"
      | 63 -> "111224"
      | 64 -> "111422"
      | 65 -> "121124"
      | 66 -> "121421"
      | 67 -> "141122"
      | 68 -> "141221"
      | 69 -> "112214"
      | 70 -> "112412"
      | 71 -> "122114"
      | 72 -> "122411"
      | 73 -> "142112"
      | 74 -> "142211"
      | 75 -> "241211"
      | 76 -> "221114"
      | 77 -> "413111"
      | 78 -> "241112"
      | 79 -> "134111"
      | 80 -> "111242"
      | 81 -> "121142"
      | 82 -> "121241"
      | 83 -> "114212"
      | 84 -> "124112"
      | 85 -> "124211"
      | 86 -> "411212"
      | 87 -> "421112"
      | 88 -> "421211"
      | 89 -> "212141"
      | 90 -> "214121"
      | 91 -> "412121"
      | 92 -> "111143"
      | 93 -> "111341"
      | 94 -> "131141"
      | 95 -> "114113"
      | 96 -> "114311"
      | 97 -> "411113"
      | 98 -> "411311"
      | 99 -> "113141"
      | 100 -> "114131" (* CODE B *)
      | 101 -> "311141" (* CODE A *)
      | 102 -> "411131" (* FNC1 *)
      | 103 -> "211412" (* START A *)
      | 104 -> "211214" (* START B *)
      | 105 -> "211232" (* START C *)
      | 106 -> "2331112" (* STOP *)
      | 107 -> "21" (* END BAR *)
      | _ -> invalid_arg "encode"

    let abc_set, a_set, b_set, c_set =
      let abc = Buffer.create 17 in
      let a = Buffer.create 17 in
      let b = Buffer.create 17 in
      for i = 32 to 95 do Buffer.add_char abc (Char.chr i) done;
      Buffer.add_buffer a abc;
      Buffer.add_buffer b abc;
      for i = 0 to 31 do
        Buffer.add_char abc (Char.chr i); line_height
        Buffer.add_char a (Char.chr i);
      done;
      for i = 96 to 126 do
        Buffer.add_char abc (Char.chr i);
        Buffer.add_char b (Char.chr i);
      done;
      abc, a, b, "0123456789"




    for ($i=0; $i<96; $i++) {                                                  // convertisseurs des jeux A & B
        @$this->SetFrom["A"] .= chr($i);
        @$this->SetFrom["B"] .= chr($i + 32);
        @$this->SetTo["A"] .= chr(($i < 32) ? $i+64 : $i-32);
        @$this->SetTo["B"] .= chr($i);
    }




    let write ~x ~y ?(height=16.) ?(width=0.35) ?(set=(`C : [`A | `B | `C])) ?text ~barcode (doc : #Fpdf.document) =
      match set with `A | `B -> invalid_arg "write"
        | `C when String.length barcode mod 2 <> 0 ->
          invalid_arg (sprintf "Fpdf_barcode.Code128-C: barcode length must be even (%s)." barcode)
        | `C ->
          let value_of_c str =
            if String.length str <> 2 then invalid_arg "value_of_c" else
              (try int_of_string str with Failure "int_of_string" -> invalid_arg "Fpdf_barcode.Code128-C: barcode must be numeric.")
          in
          let i = ref 0 in
          let len = String.length barcode in
          let values = ref [105] in (* 105 = START C *)
          while !i < len do
            values := (value_of_c (String.sub barcode !i 2)) :: !values;
            i := !i + 2;
          done;
          values := 106 :: (check_symbol !values) :: !values; (* 106 = STOP *)
          let code = String.concat "" (List.rev_map encode !values) in
          (* Draw bars *)
          let baseline = width in
          let margin = baseline *. 11. in
          let pos = ref (x +. margin) in
          for i = 0 to String.length code - 1 do
            let width = baseline *. (float_of_string (String.sub code i 1)) in
            if i mod 2 = 0 then (doc#rect ~x:!pos ~y ~width ~height ~style:`Fill ());
            pos := !pos +. width
          done;
          match text with None -> () | Some size ->
            doc#set_font ~size ();
            let text = Printf.sprintf "%s" barcode (*(check_symbol !values)*) in
            let text_width = doc#get_string_width text in
            let barcode_width = float (get_width ~barcode) *. baseline in
            let x = x +. (barcode_width -. text_width) /. 2. in
            doc#text ~x ~y:(y +. height +. (size -. 1.) /. doc#scale) ~text ()
  end





















*)






