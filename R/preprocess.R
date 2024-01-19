#' Function to remove punctuation prior to tokenising.
#'
#' @param data A corpus or list of corpuses.
#'
#' @return A cleaned corpus
remove_punctuation_v1 <- function(data){

  # Step 1:Remove the punctuation.
  clean_texts <- stringi::stri_replace_all_regex(as.character(data), "[[:punct:]]", "")
  # Step 2: Update the corpus with the cleaned texts
  result <- quanteda::corpus(clean_texts,
                             docnames = quanteda::docnames(data),
                             docvars = quanteda::docvars(data))

  return(result)
}


#' Function to remove punctuation prior to tokenising.
#'
#' @param data A corpus or list of corpuses.
#'
#' @return A cleaned corpus
remove_punctuation <- function(data) {
  # Extract the document names and docvars
  doc_names <- quanteda::docnames(data)
  all_docvars <- quanteda::docvars(data)

  # Initialize an empty list for the cleaned texts
  cleaned_texts <- vector("list", length = length(doc_names))

  # Loop over each document in the corpus
  for (i in seq_along(doc_names)) {
    # Extract the text of the current document
    doc_text <- as.character(data[[i]])

    # Remove punctuation from the text
    cleaned_texts[[i]] <- stringi::stri_replace_all_regex(doc_text, "[[:punct:]]", "")
  }

  # Create a new corpus from the cleaned texts
  cleaned_corpus <- quanteda::corpus(unlist(cleaned_texts), docnames = doc_names)

  # Assign the original docvars to the cleaned corpus
  quanteda::docvars(cleaned_corpus) <- all_docvars

  return(cleaned_corpus)
}

#' Tokenise a Corpus or List of Corpus Objects.
#'
#' @param data A corpus or list of corpus object
#' @param remove_punct_first Remove punctuation before tokenising
#' @param ... Additional arguments to quanteda tokens
#'
#' @return data
#' @export
tokenise <- function(data, remove_punct_first = FALSE, ...){
  # If data is in a list then use parallel processing.
  if(typeof(data) == "list") {
    # Set a multisession to use parallel processing
    future::plan(future::multisession)

    if(remove_punct_first == TRUE){
      # x <- remove_punctuation(data$x)
      # y <- remove_punctuation(data$y)
      #
      # data <- list(x = x, y = y)
      data <- future.apply::future_lapply(data, remove_punctuation_v1)
    }

    data <- future.apply::future_lapply(data, quanteda::tokens, ..., future.seed = 9)
  } else {

    if(remove_punct_first == TRUE){
      data <- remove_punctuation(data)
    }

    data <- quanteda::tokens(data, ...)
  }
  return(data)
}

#' Chunk a Tokenised Corpus or List of Tokenised Corpuses
#'
#' @param data A corpus or list of corpuses
#' @param ...  Additional arguments to quanteda tokens_chunk
#'
#' @return data
#' @export
chunk_tokens <- function(data, ...){
  if(typeof(data) == "list") {
    # Set a multisession to use parallel processing
    future::plan(future::multisession)
    data <- future.apply::future_lapply(data, quanteda::tokens_chunk, ...)
  } else {
    data <- quanteda::tokens_chunk(data, ...)
  }
  return(data)
}

#' Select a certain number of tokens from the corpus.
#'
#' @param data A corpus or list of corpuses.
#' @param n The number of tokens to keep
#' @param token_type The type of token to select.
#' @param ... Additional arguments for quanteda::tokens_select
#'
#' @return A corpus object
#' @export
select_n_tokens <- function(data, n = 500, token_type = "word", ...){

  if(typeof(data) == "list"){

    vars_x <- quanteda::docvars(data$x)
    vars_y <- quanteda::docvars(data$y)

    # test for different tokens count underscores
    result_x <- quanteda::tokens_select(data$x, selection = "keep", endpos = n)

    result_y <- quanteda::tokens_select(data$y, selection = "keep", startpos = -n)

    # We want to convert the results back into a corpus, pasting the tokens together first before readding docvars
    result_x <- vapply(result_x, paste, FUN.VALUE = character(1), collapse = " ") |>
      quanteda::corpus()
    quanteda::docvars(result_x) <- vars_x

    result_y <- vapply(result_y, paste, FUN.VALUE = character(1), collapse = " ") |>
      quanteda::corpus()
    quanteda::docvars(result_y) <- vars_y

    # Combine the results
    result <- list('x' = result_x, 'y' = result_y)
  } else {

    result <- quanteda::tokens_select(data, endpos = n)
  }
  return(result)
}

#' Convert Tokens into n-grams if required.
#'
#' @param data A corpus or list of corpuses
#' @param ... Additional arguments to n_gram
#'
#' @return A tokens object
#' @export
n_grams <- function(data, ...){
  # If data is in a list then use parallel processing.
  if(typeof(data) == "list") {
    # Set a multisession to use parallel processing
    future::plan(future::multisession)
    data <- future.apply::future_lapply(data, quanteda::tokens_ngrams, ..., future.seed = 9)
  } else {
    data <- quanteda::char_ngrams(data, ...)
  }
  return(data)
}


