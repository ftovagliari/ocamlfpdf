(** Print text containing markup. *)

type analysis = {
  mutable width                          : float;
  mutable height                         : float;
  mutable paragraphs                     : paragraph list;
  mutable print                          : x:float -> y:float -> ?valign:(float * float) -> unit -> unit;
}

and cell = {
  mutable text                           : string;
  mutable attr                           : attributes;
  mutable cell_width                     : float;
  mutable cell_height                    : float;
  mutable par                            : int;
  mutable line                           : int;
}

and line = {
  mutable line_width                     : float;
  mutable line_height                    : float;
  mutable line_cells                     : cell list;
}

and paragraph = {
  mutable paragraph_align                : float;
  mutable paragraph_line_spacing         : float;
  paragraph_lines                        : line list;
}

and attributes = {
  mutable family                         : Font.family;
  mutable style                          : Font.style list;
  mutable size                           : float;
  mutable scale                          : float option;
  mutable char_space                     : float option;
  mutable underline                      : [`NONE | `SINGLE | `LOW ];
  mutable color                          : string;
  mutable bgcolor                        : string option;
  mutable align                          : float;
  mutable line_spacing                   : float;
}

(** Analyze and print text containing markup.
    Recognized tags are [<span>] and [<br/>], attributes for the [<span>] tag are:

    - [family]: families are those available from the module {!Font}, names are lowercase.
    - [style]: 'italic' or 'bold' separated by comma.
    - [size]: float.
    - [scale]: Horizontal scaling, default is 100 (integer).
    - [line_spacing]: float, default is 1x.
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
    @param line_height Height of the line of text.
    @param markup Text with markup.
    @param bgcolor Background color.
    @param border_width Border width.
    @param border_color Border color.
    @param border_radius Radius for boxes with rounded corners.
    @param padding Top, right, bottom and left internal padding.
    @return The actual width and height of the markup box.
    @raise Error(Invalid_markup) If an error occurs.
*)
val prepare :
  width:float ->
  (*line_height:float ->*)
  markup:string ->
  (*?align:[> `Left ] ->*)
  ?bgcolor:string ->
  ?border_width:float ->
  ?border_color:string ->
  ?border_radius:float ->
  ?padding:(float * float * float * float) ->
  PDF.t -> analysis

(** Print text containing markup.
    Recognized tags are [<span>] and [<br/>], attributes for the [<span>] tag are:

    - [family]: families are those available from the module {!Font}, names are lowercase.
    - [style]: 'italic' or 'bold' separated by comma.
    - [size]: float.
    - [scale]: Horizontal scaling, default is 100 (integer).
    - [line_spacing]: float, default is 1x.
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
    @raise Error(Invalid_markup) If an error occurs.
    @deprecated Use {!PDFMarkup.prepare}
*)
val print :
  x:float ->
  y:float ->
  width:float ->
  (*line_height:float ->*)
  markup:string ->
  (*?align:[> `Left ] ->*)
  ?bgcolor:string ->
  ?border_width:float ->
  ?border_color:string ->
  ?border_radius:float ->
  ?padding:(float * float * float * float) ->
  PDF.t -> float * float
