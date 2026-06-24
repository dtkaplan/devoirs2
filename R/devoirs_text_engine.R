#' Knitr engine devoirs_text for adding a free text entry widget to the document.
#'
#' This should *not* be called directly by the user.
#'
devoirs_text_engine <- function(options) {
  current_file <- knitr::current_input()
  if (!is.null(current_file)) devoirs_state$source_file <- current_file
  clean_lines <- options$code[!grepl("^\\s*#\\|", options$code)]
  chunk_points <- if (!is.null(options$points)) as.numeric(options$points) else 1.0
  placeholder_text <- paste(clean_lines, collapse = "\n")
  placeholder_text <- gsub('"', '&quot;', placeholder_text, fixed = TRUE)
  chunk_label <- options$label
  is_multiline <- !is.null(options$multiline) && (options$multiline == TRUE || options$multiline == "true")
  box_style <- "width: 100%;"
  if (!is.null(options$devoirs_width)) box_style <- paste0("width: ", options$devoirs_width, "em;")
  input_element_id <- paste0("txt_input_", chunk_label)
  devoirs_state$registered_text[[chunk_label]] <- list(label = chunk_label, points = chunk_points)
  html_out <- c(paste0("<div class='devoirs-text-item' style='margin-bottom:25px;'>"))
  if (is_multiline) {
    html_out <- c(html_out, paste0("  <textarea id='", input_element_id, "' placeholder='", placeholder_text, "' style='", box_style, " min-height:100px; padding:10px; border:1px solid #ccc; border-radius:4px; font-family:inherit;' oninput=\"localStorage.setItem('devoirs_text_' + '", chunk_label, "', this.value)\"></textarea>"))
  } else {
    html_out <- c(html_out, paste0("  <input type='text' id='", input_element_id, "' placeholder='", placeholder_text, "' style='", box_style, " padding:10px; border:1px solid #ccc; border-radius:4px;' oninput=\"localStorage.setItem('devoirs_text_' + '", chunk_label, "', this.value)\">"))
  }
  html_out <- c(html_out, "</div>", "")
  options$echo <- FALSE; options$results <- "asis"
  return(knitr::engine_output(options, clean_lines, out = paste(html_out, collapse = "\n")))
}
