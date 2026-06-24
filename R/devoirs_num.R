#' Add a widget that asks for numerical input
#'
#' Use this in an inline chunk
#' @export
devoirs_num <- function(label, true_answer, interval = 0, devoirs_feedback = TRUE, placeholder = "num...", width = 6, points = 1) {
  current_file <- knitr::current_input()
  if (!is.null(current_file)) devoirs_state$source_file <- current_file
  num_meta <- list(id = label, trueAnswer = true_answer, interval = interval, points = as.numeric(points))
  devoirs_state$registered_nums[[label]] <- num_meta
  html_string <- paste0(
    "<input type='text' id='n_input_", label, "' placeholder='", placeholder, "' ",
    "style='width: ", width, "em; display: inline-block; padding: 4px 8px; ",
    "border: 1px solid #ccc; border-radius: 4px; margin: 0 4px; vertical-align: middle; font-family: inherit; font-size: inherit;' ",
    "oninput=\"localStorage.setItem('devoirs_num_' + '", label, "', this.value); ",
    "          if (", tolower(devoirs_feedback), ") { ",
    "            var valStr = this.value.trim(); ",
    "            if (valStr === '') { ",
    "              this.style.backgroundColor = ''; this.style.borderColor = '#ccc'; return; ",
    "            } ",
    "            var parsedNum = Number(valStr); ",
    "            if (isNaN(parsedNum)) { ",
    "              this.style.backgroundColor = '#d5a6bd'; ",
    "              this.style.borderColor = '#a64d79'; ",
    "            } else { ",
    "              var target = ", true_answer, "; ",
    "              var allowedDiff = ", interval, "; ",
    "              var isWithinInterval = Math.abs(parsedNum - target) <= allowedDiff; ",
    "              this.style.backgroundColor = isWithinInterval ? '#e2f0d9' : '#fce4d6'; ",
    "              this.style.borderColor = isWithinInterval ? '#70ad47' : '#c00000'; ",
    "            } ",
    "          }\">"
  )
  return(knitr::asis_output(html_string))
}


