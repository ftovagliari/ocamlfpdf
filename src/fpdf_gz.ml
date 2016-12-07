(** Compression *)
let gz_input source i =
  let source_length = String.length source in
  fun buf ->
    let len = min (String.length buf) (source_length - !i) in
    String.blit source !i buf 0 len ;
    i := !i + len;
    len
;;

(** gz_compress *)
let compress txt =
  let i = ref 0 in
  let res = Buffer.create (String.length txt * 8 / 10) in
  let input = gz_input txt i in
  let output buf len = Buffer.add_string res (String.sub buf 0 len) in
  Zlib.compress input output;
  Buffer.contents res
;;

(** gz_uncompress *)
let uncompress txt =
  let i = ref 0 in
  let res = Buffer.create (String.length txt * 10 / 8) in
  let input = gz_input txt i in
  let output buf len = Buffer.add_string res (String.sub buf 0 len) in
  Zlib.uncompress input output;
  Buffer.contents res
;;

