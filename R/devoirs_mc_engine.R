#' Defines a knitr engine for multiple choice questions.
#'
#' This should *not* be called directly by the user.
devoirs_mc_engine <- function(options) {
  current_file <- knitr::current_input()
  if (!is.null(current_file)) devoirs_state$source_file <- current_file
  clean_lines <- options$code[!grepl("^\\s*#\\|", options$code)]
  yaml_text <- paste(clean_lines, collapse = "\n")
  quiz_items <- yaml::yaml.load(yaml_text)
  chunk_label <- options$label
  fmt <- if (!is.null(options$devoirs_format)) options$devoirs_format else "html"
  show_feedback <- !identical(options$devoirs_feedback, FALSE) && !identical(options$devoirs_feedback, "false")
  chunk_data_list <- list()
  output_buffer <- c()
  for (i in seq_along(quiz_items)) {
    item <- quiz_items[[i]]
    item_correct_choices <- c()
    item_id <- paste0(chunk_label, "_item_", i)
    feedback_container_id <- paste0("fb_lbl_", item_id)
    item_points <- if (!is.null(item$points)) as.numeric(item$points) else 1.0
    total_item_correct <- sum(sapply(item$choices, function(x) isTRUE(x$correct) || identical(x$correct, "true")))
    input_type <- if (total_item_correct > 1) "checkbox" else "radio"
    if (fmt == "markdown") {
      # output_buffer <- c(output_buffer, "::: {layout-ncol=2}", "")
      for (j in seq_along(item$choices)) {
        choice_node <- item$choices[[j]]
        is_correct <- isTRUE(choice_node$correct) || identical(choice_node$correct, "true")
        alpha_prefix <- paste0(letters[j], ".")
        if (show_feedback && is_correct) {
          output_buffer <- c(output_buffer, paste0("- ✓ ", alpha_prefix, " ", choice_node$choice))
          if (!is.null(choice_node$feedback_message)) output_buffer <- c(output_buffer, paste0("  *Feedback: ", choice_node$feedback_message, "*"))
        } else {
          output_buffer <- c(output_buffer, paste0("- ", alpha_prefix, " ", choice_node$choice))
        }
      }
      # output_buffer <- c(output_buffer, "", ":::", "")
    } else {
      output_buffer <- c(output_buffer,
                         paste0("<div class='devoirs-mc-item' style='margin-bottom:25px; padding:5px; background:#fafafa;'>"),
                         paste0("  <div style='margin-bottom:2px; min-height: 1px;'><div id='", feedback_container_id, "' style='font-size: 0.95em;'></div></div>"),
                         paste0("  <form id='form_", item_id, "' data-chunk-label='", chunk_label, "' data-item-id='", item_id, "'>")
      )
      for (j in seq_along(item$choices)) {
        choice_node <- item$choices[[j]]
        choice_id <- choice_node$id
        is_correct <- isTRUE(choice_node$correct) || identical(choice_node$correct, "true")
        if (is_correct) item_correct_choices <- c(item_correct_choices, choice_id)
        html_choice <- process_math_markdown(choice_node$choice)
        fb_msg_html <- process_math_markdown(choice_node$feedback_message)
        fb_msg_html <- gsub("'", "\\'", fb_msg_html, fixed = TRUE)
        output_buffer <- c(output_buffer, paste0(
          "    <label style='display:flex; align-items:center; gap:8px; margin-bottom:6px; cursor:pointer;'>",
          "      <input type='", input_type, "' name='", item_id, "' value='", choice_id, "' ",
          "             data-correct='", is_correct, "' data-feedback='", fb_msg_html, "' ",
          "             onchange=\"var f=this.form; var c=Array.from(f.querySelectorAll('input:checked')); ",
          "                       var fb=document.getElementById('", feedback_container_id, "'); ",
          "                       var storageKey = 'devoirs_mc_' + '", chunk_label, "_' + this.value; ",
          "                       localStorage.setItem(storageKey, this.checked); ",
          "                       if(!", tolower(show_feedback), ") return; ",
          "                       if(c.length===0){ fb.innerHTML=''; return; } ",
          # the 'darkgray' below used to be simply textColor
          "                       var msgs = c.map(i => { ",
          "                         var attrVal = i.getAttribute('data-correct') || 'false'; ",
          "                         var isCorr = attrVal.toLowerCase() === 'true'; ",
          "                         var icon = isCorr ? '<span style=\\'color:green; font-weight:bold;\\'>✓ </span>' : '<span style=\\'color:red; font-weight:bold;\\'>✗ </span>'; ",
          "                         var textColor = isCorr ? 'green' : 'red'; ",
          "                         var txt = i.getAttribute('data-feedback'); ",
          "                         return '<div style=\\'margin-bottom:4px; color:' + 'darkgray' + ';\\'>' + icon + (txt ? txt : 'Choice selected.') + '</div>'; ",
          "                       }); ",
          "                       fb.innerHTML = msgs.join(''); ",
          "                       if (window.MathJax && window.MathJax.typesetPromise) { MathJax.typesetPromise([fb]); } \"> ",
                "      <div>", html_choice, "</div>",
          "    </label>"
        ))
      }
      output_buffer <- c(output_buffer, "  </form>", "</div>", "")
      item_meta <- list(itemId = item_id, totalCorrect = length(item_correct_choices), points = item_points)
      chunk_data_list[[length(chunk_data_list) + 1]] <- item_meta
    }
  }
  if (fmt != "markdown") devoirs_state$registered_mc[[chunk_label]] <- chunk_data_list
  options$echo <- FALSE; options$results <- "asis"
  return(knitr::engine_output(options, clean_lines, out = paste(output_buffer, collapse = "\n")))
}
