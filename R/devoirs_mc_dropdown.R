#' An inline multiple choice question in dropdown menu format
#'
#' Use this in an inline chunk.
#'
#' @export
devoirs_mc_dropdown <- function(label, choices, correct, points = 1, devoirs_feedback = TRUE) {
  current_file <- knitr::current_input()
  if (!is.null(current_file)) devoirs_state$source_file <- current_file

  # Ensure the correct target answer sits cleanly as an escaped string character variable
  correct_clean <- gsub('"', '\\"', as.character(correct), fixed = TRUE)

  # Save the dropdown structure options and scoring parameters globally
  set_meta <- list(id = label, correct = correct_clean, points = as.numeric(points))
  devoirs_state$registered_sets[[label]] <- set_meta

  # Map option tag components from the R choices vector
  option_elements <- c("<option value=''>-- select --</option>")
  for (choice in choices) {
    choice_str <- gsub('"', '&quot;', as.character(choice), fixed = TRUE)
    option_elements <- c(option_elements, paste0("<option value='", choice_str, "'>", choice_str, "</option>"))
  }
  options_compiled <- paste(option_elements, collapse = "\n")

  # Generate inline dropdown HTML elements with live adjacent feedback indicators
  html_string <- paste0(
    "<span style='display: inline-flex; align-items: center; gap: 4px; vertical-align: middle;'>",
    "  <select id='s_input_", label, "' ",
    "          style='padding: 4px 8px; border: 1px solid #ccc; border-radius: 4px; font-family: inherit; font-size: inherit; background: white;' ",
    "          onchange=\"localStorage.setItem('devoirs_set_' + '", label, "', this.value); ",
    "                    var iconSpan = document.getElementById('s_fb_", label, "'); ",
    "                    if (!", tolower(devoirs_feedback), ") return; ",
    "                    if (this.value === '') { iconSpan.innerHTML = ''; return; } ",
    "                    var isCorr = this.value.trim().toLowerCase() === '", tolower(correct_clean), "'.trim().toLowerCase(); ",
    "                    if (isCorr) { ",
    "                      iconSpan.innerHTML = '✓'; iconSpan.style.color = 'green'; ",
    "                    } else { ",
    "                      iconSpan.innerHTML = '✗'; iconSpan.style.color = 'red'; ",
    "                    }\">",
    options_compiled,
    "  </select>",
    "  <span id='s_fb_", label, "' style='font-weight: bold; font-size: 1.1em;'></span>",
    "</span>"
  )
  return(knitr::asis_output(html_string))
}
