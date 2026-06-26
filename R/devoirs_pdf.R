#' Versions of the devoirs inline widgets suited for PDF output
#'
#' NOTE IN DRAFT: Go back and rename the original `devoirs_set()` and
#' similar functions to `devoirs_set_html()` and such.
#' Then, write your own `devoirs_set()` that will look at
#' the target document format and
#' choose either the `_html` or `_pdf` version as appropriate.
#'
#' @export
devoirs_word_pdf <- function(id, placeholder = "", width = 10, true_answer = "",
                             devoirs_feedback = knitr::opts_chunk$get("devoirs_feedback"),
                             points = 1) {
  if (is.null(devoirs_feedback)) devoirs_feedback = TRUE
  # Generate a clean LaTeX underlining spacer rule to serve as a handwriting blank
  # Roughly mapping em width to a standard LaTeX underline width
  pt_width <- width * 6
  pdf_blank <- paste0("\\underline{\\hspace{", pt_width, "pt}}")

  if (isTRUE(devoirs_feedback) && true_answer != "") {
    # If feedback mode is turned on, display the correct string adjacent in italics
    return(knitr::asis_output(paste0(pdf_blank, " *(Answer: ", true_answer, ")*")))
  } else {
    return(knitr::asis_output(pdf_blank))
  }
}

#' @export
devoirs_num_pdf <- function(label, true_answer, interval = 0,
                            devoirs_feedback = knitr::opts_chunk$get("devoirs_feedback"),
                            placeholder = "num...", width = 6, points = 1) {
  if (is.null(devoirs_feedback)) devoirs_feedback = TRUE
  # Generate a compact numeric LaTeX handwriting underline path spacer
  pt_width <- width * 6
  pdf_blank <- paste0("\\underline{\\hspace{", pt_width, "pt}}")

  if (isTRUE(devoirs_feedback)) {
    # If feedback mode is turned on, print target number and optional range tolerances
    feedback_str <- paste0(" *(Answer: ", true_answer)
    if (interval > 0) {
      feedback_str <- paste0(feedback_str, " \\pm ", interval)
    }
    feedback_str <- paste0(feedback_str, ")*")
    return(knitr::asis_output(paste0(pdf_blank, feedback_str)))
  } else {
    return(knitr::asis_output(pdf_blank))
  }
}

#' @export
devoirs_set_pdf <- function(label, choices, correct, points = 1,
                            devoirs_feedback = knitr::opts_chunk$get("devoirs_feedback")) {
  if (is.null(devoirs_feedback)) devoirs_feedback = TRUE
  formatted_choices <- c()

  for (choice in choices) {
    choice_str <- as.character(choice)
    # Check case-independent structural equality matching
    is_correct_choice <- tolower(trimws(choice_str)) == tolower(trimws(as.character(correct)))

    if (isTRUE(devoirs_feedback) && is_correct_choice) {
      # If feedback mode is turned on, wrap the targeted correct entry inside markdown underlining tags
      formatted_choices <- c(formatted_choices, paste0("<u>", choice_str, "</u>"))
    } else {
      formatted_choices <- c(formatted_choices, choice_str)
    }
  }

  # Concatenate options separated by the custom delimiter string matrix
  compiled_set_string <- paste0("{", paste(formatted_choices, collapse = " :: "), "}")
  return(knitr::asis_output(compiled_set_string))
}
