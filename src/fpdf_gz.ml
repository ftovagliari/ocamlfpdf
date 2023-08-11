(** Compression *)
let gz_input source i =
  let source_length = Bytes.length source in
  fun buf ->
    let len = min (Bytes.length buf) (source_length - !i) in
    Bytes.blit source !i buf 0 len ;
    i := !i + len;
    len
;;

(** gz_compress *)
let compress txt =
  let bytes = Bytes.of_string txt in
  let i = ref 0 in
  let res = Buffer.create (Bytes.length bytes * 8 / 10) in
  let input = gz_input bytes i in
  let output buf len = Buffer.add_bytes res (Bytes.sub buf 0 len) in
  Zlib.compress input output;
  Buffer.contents res
;;

(** gz_uncompress *)
let uncompress txt =
  let bytes = Bytes.of_string txt in
  let i = ref 0 in
  let res = Buffer.create (Bytes.length bytes * 10 / 8) in
  let input = gz_input bytes i in
  let output buf len = Buffer.add_bytes res (Bytes.sub buf 0 len) in
  Zlib.uncompress input output;
  Buffer.contents res
;;

