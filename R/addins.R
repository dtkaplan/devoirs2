#' insert {devoirs} elements
#'
#' @export
insert_devoirs_mc <- function(){
  this_doc <- rstudioapi::getSourceEditorContext()
  contents <- this_doc$selection
  highlighted <- contents |> rstudioapi::primary_selection()
  # itemlabel <- new_item_label(this_doc$path)
  itemlabel <- stringi::stri_rand_strings(1, 5, pattern = "[a-zA-Z]")
  rstudioapi::insertText(
    contents$range,
    new_mc_content(highlighted$text, itemlabel),
    id = this_doc$id)
}



new_mc_content <- function(text, label) {
  mc_template <- r"(```{{devoirs_mc}}
#| label: {label}
#| devoirs_format: html
#| devoirs_feedback: true
- choices:
    - id: "a"
      choice: "Choice a"
      feedback_message: "Nice"
      correct: true
    - id: "b"
      choice: "Choice b"
      correct: false
      feedback_message: "Sorry."

```
)"

  glue::glue(mc_template)
}
