#' Initialize a Quarto/HTML assignment document
#'
#' Starts the ball rolling on a new assignment document. Creates the button
#' that, when pressed by the reader of the assignment, will collect of the the
#' answers to `{devoirs}` items.
#'
#' @param clear_cache If `TRUE` will add a button to reset the document's
#' devoirs answer cache
#'
#' @export
devoirs_init <- function(clear_cache = TRUE) {
  # Register the knitr engines and hooks
  if (!exists("devoirs_state")) {
    devoirs_state <<- new.env(parent = emptyenv())
    devoirs_state$registered_mc <- list()
    devoirs_state$registered_text <- list()
    devoirs_state$registered_words <- list()
    devoirs_state$registered_nums <- list()
    devoirs_state$registered_sets <- list()
    devoirs_state$source_file <- "unknown.qmd"
  }

  knitr::knit_engines$set(devoirs_mc = devoirs_mc_engine)
  knitr::knit_engines$set(devoirs_text = devoirs_text_engine)
  knitr::knit_hooks$set(document = devoirs_knitr_hooks)

  devoirs_submit_button(clear_cache = clear_cache)
}

#' @export
devoirs_submit_button <- function(clear_cache = TRUE ){
  collect_string <-
  '<button type="button" onclick="processDevoirsSubmission()" style="padding: 12px 24px; font-weight: bold; background: #6f42c1; color: white; border: none; border-radius: 4px; cursor: pointer; margin-bottom: 30px;">
  Collect answers for submission
</button>
  <!-- EXPORT_SCRIPT_PLACEHOLDER -->'

  # The HTML comment above, EXPORT_SCRIPT_PLACEHOLDER, is critical. That's what get the
  # JavaScript for collecting and answers inserted into the document.

  clear_cache_string <-
  '<button type="button" onclick="clearDevoirsCache()" style="padding: 12px 24px; font-weight: bold; background: #c00000; color: white; border: none; border-radius: 4px; cursor: pointer; margin-left: 10px;">Clear answer memory</button>
'

  if (!clear_cache) collect_string |> knitr::asis_output()
  else {
    paste(collect_string, clear_cache_string) |>
      knitr::asis_output()
  }
}
