(** Print text containing markup. *)

(** Print text containing markup.
    Recognized tags are [<span>] and [<br/>], attributes for the [<span>] tag are:

    - [family]: families are those available from the module {!Font}, names are lowercase.
    - [style]: 'italic' or 'bold' separated by comma.
    - [size]: float.
    - [scale]: Horizontal scaling, default is 100 (integer).
    - [align]: A float between 0.0 and 1.0.
    - [underline]: 'none', 'single', 'low'.
    - [color]: HEX triplet
    - [bgcolor]: HEX triplet

    You can not nest multiple [<span>] tags.

    Example:
    {[<span bgcolor='#f0f0ff' color='#ff1010' family='courier' size='12.5' style='italic,bold'>...</span>]}

    @param x Absolute abscissa of the upper-left corner.
    @param y Absolute ordinate of the upper-left corner.
    @param width Total width of the box.
    @param height The height with respect to which the resulting box is vertically aligned.
    @param line_height Height of the line of text.
    @param markup Text with markup.
    @param bgcolor Background color.
    @param border_width Border width.
    @param border_color Border color.
    @param border_radius Radius for boxes with rounded corners.
    @param padding Top, right, bottom and left internal padding.
    @param valign Vertical alignment of the box with respect to [height]. If [height]
    @return The actual width and height of the markup box.
    @raise Failure("invalid_markup") If an error occurs.
*)
val print :
  x:float ->
  y:float ->
  width:float ->
  ?height:float ->
  (*line_height:float ->*)
  markup:string ->
  (*?align:[> `Left ] ->*)
  ?bgcolor:string ->
  ?border_width:float ->
  ?border_color:string ->
  ?border_radius:float ->
  ?padding:(float * float * float * float) ->
  ?valign:float -> PDF.t -> float * float
