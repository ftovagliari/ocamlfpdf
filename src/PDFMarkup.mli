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
  mutable font_metrics                   : Font.t;
  mutable cell_width                     : float;
  mutable cell_height                    : float;
  mutable par                            : int;
  mutable line                           : int;
}

and line = {
  mutable line_width                     : float;
  mutable line_height                    : float;
  mutable line_max_font_size             : (Font.t * float);
  mutable line_spacing                   : float;
  mutable line_cells                     : cell list;
}

and paragraph = {
  mutable paragraph_align                : float;
  paragraph_lines                        : line list;
}

and attributes = {
  mutable family                         : Font.family;
  mutable style                          : Font.style list;
  mutable size                           : float;
  mutable scale                          : float option;
  mutable char_space                     : float option;
  mutable rise                           : float option;
  mutable underline                      : [`NONE | `SINGLE | `LOW ];
  mutable color                          : string;
  mutable bgcolor                        : string option;
  mutable align                          : float;
  mutable lspacing                       : float;
}

(** Analyzes text containing markup and returns a {!PDFMarkup.analysis} record.
    The markup can be actually printed to the PDF document through the field [print] of
    the analysis record.

    Recognized tags are [<SPAN>] and [<BR/>]. Attributes for the [<SPAN>] tag are:

    - [family]: Families are those available from the module [Font.family], names are case sensitive.
    - [style]: ['italic'] and/or ['bold'], separated by comma.
    - [size]: Font size (float).
    - [scale]: Horizontal scaling, which is a number specifying the percentage of the normal width. Default is [100] (integer).
    - [char_space]: Character spacing, which is a number expressed in unscaled text space units. Default is [0.0] (float).
    - [line_spacing]: Factor by which the font height is multiplied. Default is [1.0] (float).
    - [align]: A float between [0.0] and [1.0].
    - [underline]: One of ['none'], ['single'], ['low'].
    - [color]: HEX triplet
    - [bgcolor]: HEX triplet

    Nesting multiple [<SPAN>] tags is not permitted.

    Example:
    {[<SPAN bgcolor='#f0f0ff' color='#ff1010' family='Courier' size='12.5' style='italic,bold'>...</SPAN>]}

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
    @return A {!PDFMarkup.analysis} record.
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
  ?line_spacing:float ->
  PDF.t -> analysis

(**
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
