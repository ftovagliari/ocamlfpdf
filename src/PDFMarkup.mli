(** Text with markup *)

val rgb_of_hex : string -> int * int * int
val hex_of_rgb : int * int * int -> string

(** Print text with markup.
    Tags are [<span>] and [<br/>]; attributes for the
    [<span>] tag are:

    {ul {- [family]: families are those available from the module {!Font}, names are lowercase.}
    {- [style]:      'underline', 'italic', 'bold'.}
    {- [size]:       float.}
    {- [underline]:  'none', 'single', 'low'.}
    {- [color]:      HEX triplet}
    {- [bgcolor]:    HEX triplet }}

    Example:

    {[  <SPAN bgcolor='#f0f0ff' color='#ff1010' family='courier' size='12.5'>...</SPAN>]}
    @param x Absolute abscissa of the upper-left corner.
    @param y Absolute ordinate of the upper-left corner.
    @param width Total width of the box.
    @param line_height Height of the line of text.
    @param markup Text with markup.
    @param bgcolor Background color.
    @param border_width Border width.
    @param border_color Border color.
    @param border_radius Radius for boxes with rounded corners.
    @param padding Internal padding.
  *)
val print :
  x:float ->
  y:float ->
  width:float ->
  line_height:float ->
  markup:string ->
  (*?align:[> `Left ] ->*)
  ?bgcolor:string ->
  ?border_width:float ->
  ?border_color:string ->
  ?border_radius:float ->
  ?padding:float -> PDF.document -> float * float
