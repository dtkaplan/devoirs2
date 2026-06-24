#' Internals for the devoirs2 package
#'
#' These functions are invoked automatically during the construction of
#' a Quarto/devoirs2 document. They provide the necessary JavaScript functionality
#'
#' Replaces the LaTeX dollar signs in a string to be displayed as feedback
#' so that MathJax will process the string.
process_math_markdown <- function(text_string) {
  if (is.null(text_string) || text_string == "") return("")
  html_output <- commonmark::markdown_html(text_string)
  html_output <- gsub("^<p>|</p>\\s*$", "", html_output)
  html_output <- gsub("\\\\{2,}", "\\\\", html_output)
  return(html_output)
}


