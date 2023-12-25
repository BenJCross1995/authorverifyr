#--------------------------------------------------------------------------------#
#------------------------------THE IMPOSTOR METHOD-------------------------------#
#--------------------------------------------------------------------------------#

#' Create character n-grams. n characters no spaces or words <= n characters long.
#'
#' @param data A corpus or list of corpuses
#' @param n_char Number of characters in character n-gram.
#'
#' @return tokenised character n-gram data
character_n_grams <- function(data, n_char = 4, ...){

  # Want to keep the words with man n_char characters and then n_grams up to n_char.
  word_dfm <- quanteda::dfm(quanteda::tokens_select(data, max_nchar = n_char))
  char_dfm <- quanteda::dfm(quanteda::char_ngrams(as.character(data), n = n_char))

  # Now remove any character n-grams containing spaces, denoted by _
  char_dfm <- quanteda::dfm_remove(char_dfm, "_", value_type = "regex")

  # Bind them together
  result <- cbind(word_dfm, char_dfm)

  return(result)

  }

#' Impostor Method
#'
#' @param data Corpus or list of corpuses
#' @param remove_punct_first remove punctuation before tokenising T or F
#' @param ... Additional quanteda arguments
#'
#' @return A result
#' @export
impostor_method <- function(data, remove_punct_first = TRUE, ...){

  # We tokenise the corpus, removing punctuation first if user desires
  tokenised_corpus <- authorverifyr::tokenise(data, remove_punct_first = remove_punct_first)

  # In the paper the authors select n tokens, default is 500
  n_tokens <- authorverifyr::select_n_tokens(tokenised_corpus)

  x <- character_n_grams(n_tokens$x)
  y <- character_n_grams(n_tokens$y)

  result <- list(x,y)

  return(result)

}
