val rgb_of_hex : string -> int * int * int
val hex_of_rgb : int * int * int -> string
val print :
  x:float ->
  y:float ->
  width:float ->
  line_height:float ->
  markup:string ->
  (*?align:[> `Left ] ->*)
  ?padding:float ->
  ?bgcolor:string ->
  ?border_width:float ->
  ?border_color:string ->
  ?border_radius:float -> PDF.document -> float * float
