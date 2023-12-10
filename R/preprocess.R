#' Tokenise a Corpus or List of Corpus Objects.
#'
#' @param data A corpus or list of corpus object
#' @param ... Additional arguments to quanteda tokens
#'
#' @return data
#' @export
tokenise <- function(data, ...){
  # If data is in a list then use parallel processing.
  if(typeof(data) == "list") {
    # Set a multisession to use parallel processing
    future::plan(future::multisession)
    data <- future.apply::future_lapply(data, quanteda::tokens, ..., future.seed = 9)
  } else {
    data <- quanteda::tokens(data, ...)
  }
  return(data)
}

