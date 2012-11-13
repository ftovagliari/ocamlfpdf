exception Continue
val fread : string -> Buffer.t
val re_cr : Str.regexp
val fixpoint : ('a -> 'a) -> 'a -> 'a
val trim : string -> string
val rtrim : string -> string
val ltrim : string -> string
val may : f:('a -> 'b) -> 'a option -> unit
val remove_dupl : 'a list -> 'a list
val gz_compress : 'a -> 'a
val gz_uncompress : 'a -> 'a
val escape : string -> string
val rgb_of_hex : string -> int * int * int
val hex_of_rgb : int * int * int -> string
val rg_of_hex : string -> string
val pdf_string : string -> string
val call_and_restore :
  pre:(unit -> 'a) -> ('b -> 'c) -> 'b -> post:('a -> 'd) -> 'c
val utf8_to_utf16 : string -> string
val memo : f:('a -> 'b) -> ?force:('b -> bool) -> 'a -> 'b
val group_by : ('a -> 'b) -> 'a list -> ('b * 'a list) list
