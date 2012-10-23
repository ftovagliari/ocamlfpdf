(** Interactive Forms.

    A collection of fields for gathering information interactively from the user. *)


type field

(**
  @param x Absolute abscissa of the upper-left corner.
  @param y Absolute ordinate of the upper-left corner.
  @param width Width of the text field.
  @param name Name of the text field.
  @param height Height of the text field. Default is the font size.
  @param border Optional border around the text field.
  @param bgcolor Background color.
  @param fgcolor Text color.
  @param font_family Font family.
  @param font_size Font size.
  @param font_style Font size.
  @param maxlength The maximum length of the field's text, in characters.
  @param readonly Whether the user may change the value of the field.
  @param numeric Whether the field accept numeric values.
  @param tooltip Field tooltip.
  @param value Field value
  @param default_value Default value.
  @param parent Parent element.
  @return An object representing the text field, for use in subsequent calls as parameter "parent" to append child elements.
*)
val add_text_field :
  x:float ->
  y:float ->
  width:float ->
  name:string ->
  ?height:float ->
  ?border:[ `Dashed | `Solid | `Underline ] * string ->
  ?bgcolor:string ->
  ?fgcolor:string ->
  ?font_family:Font.family option ->
  ?font_size:float ->
  ?font_style:Font.style list ->
  ?maxlength:int ->
  ?readonly:bool ->
  ?numeric:bool ->
  ?tooltip:string ->
  ?value:string -> ?default_value:string -> ?parent:field -> PDF.t -> field
