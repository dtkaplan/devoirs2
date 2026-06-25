# R/zzz.R

# 1. Create the private tracking environment for items in the package
# This object is visible to all functions inside your package, but hidden from users.
if (!exists("devoirs_state")) {
  devoirs_state <- new.env(parent = emptyenv())
  devoirs_state$registered_mc <- list()
  devoirs_state$registered_text <- list()
  devoirs_state$registered_words <- list()
  devoirs_state$registered_nums <- list()
  devoirs_state$registered_sets <- list()
  devoirs_state$source_file <- "unknown.qmd"
}

# 2. Provide internal helper functions to interact with the environment
# These make it easy for your other package functions to get, set, or clear components.

set_devoirs_state_component <- function(key, value) {
  assign(key, value, envir = devoirs_state)
}

get_devoirs_state_component <- function(key) {
  if (!exists(key, envir = devoirs_state, inherits = FALSE)) {
    return(NULL) # Return NULL or throw an error if it doesn't exist
  }
  get(key, envir = devoirs_state)
}

list_components <- function() {
  names(devoirs_state)
}
