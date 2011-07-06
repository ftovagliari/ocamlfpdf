exception Continue
val fread : string -> Buffer.t
val re_cr : Str.regexp
val trim : string -> string
val rtrim : string -> string
val ltrim : string -> string
val may : f:('a -> 'b) -> 'a option -> unit
val remove_dupl : 'a list -> 'a list
val gz_compress : string -> string
val gz_uncompress : string -> string
val escape : string -> string
val pdf_string : string -> string
val call_and_restore :
  pre:(unit -> 'a) -> ('b -> 'c) -> 'b -> post:('a -> 'd) -> 'c
val utf8_to_utf16 : string -> string
val memo : f:('a -> 'b) -> ?force:('b -> bool) -> 'a -> 'b
