#' Add a widget suited for a single word
#'
#' Use this as an inline function
#' @export
devoirs_word <- function(id, placeholder = "",
                         true_answer = "",
                         width = nchar(true_answer),
                         devoirs_feedback = knitr::opts_chunk$get("devoirs_feedback"),
                         points = 1) {
  if (is.null(devoirs_feedback)) devoirs_feedback = TRUE
  current_file <- knitr::current_input()
  if (!is.null(current_file)) devoirs_state$source_file <- current_file
  placeholder_clean <- gsub('"', '&quot;', placeholder, fixed = TRUE)
  true_answer_clean <- gsub('"', '\\"', true_answer, fixed = TRUE)
  word_meta <- list(id = id, trueAnswer = true_answer_clean, points = as.numeric(points))
  devoirs_state$registered_words[[id]] <- word_meta
  html_string <- paste0(
    "<input type='text' id='w_input_", id, "' placeholder='", placeholder_clean, "' ",
    "style='width: ", width, "em; display: inline-block; padding: 4px 8px; ",
    "border: 1px solid #ccc; border-radius: 4px; margin: 0 4px; vertical-align: middle; font-family: inherit; font-size: inherit;' ",
    "oninput=\"localStorage.setItem('devoirs_word_' + '", id, "', this.value); ",
    "          if (", tolower(devoirs_feedback), ") { ",
    "            var userVal = this.value.trim().toLowerCase(); ",
    "            var trueVal = '", tolower(true_answer_clean), "'.trim().toLowerCase(); ",
    "            var isCorrect = userVal === trueVal; ",
    "            if (this.value.trim() === '') { ",
    "              this.style.backgroundColor = ''; this.style.borderColor = '#ccc'; ",
    "            } else { ",
    "              this.style.backgroundColor = isCorrect ? '#e2f0d9' : '#fce4d6'; ",
    "              this.style.borderColor = isCorrect ? '#70ad47' : '#c00000'; ",
    "            } ",
    "          }\">"
  )
  return(knitr::asis_output(html_string))
}
