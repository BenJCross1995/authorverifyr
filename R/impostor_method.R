#--------------------------------------------------------------------------------#
#------------------------------THE IMPOSTOR METHOD-------------------------------#
#--------------------------------------------------------------------------------#

#' Create character n-grams. n characters no spaces or words <= n characters long.
#'
#' @param data A corpus or list of corpuses
#' @param n_char Number of characters in character n-gram.
#' @param ... Additional quanteda arguments.
#'
#' @return tokenised character n-gram data
character_n_grams <- function(data, n_char = 4, ...){

  # Want to keep the words with man n_char characters and then n_grams up to n_char.
  word_dfm <- quanteda::dfm(quanteda::tokens_select(data, max_nchar = n_char))
  # char_dfm <- quanteda::dfm(quanteda::char_ngrams(as.character(data), n = n_char))

  # Now remove any character n-grams containing spaces, denoted by _
  # char_dfm <- quanteda::dfm_remove(char_dfm, "_", value_type = "regex")

  # Bind them together
  # result <- cbind(word_dfm, char_dfm)

  result <- word_dfm
  return(result)

  }

#' Impostor Method
#'
#' @param data Corpus or list of corpuses
#' @param remove_punct_first remove punctuation before tokenising T or F
#' @param num_features Number of features to keep
#' @param num_impostors Number of impostor documents to select
#' @param ... Additional quanteda arguments
#'
#' @return A result
#' @export
impostor_method <- function(data, remove_punct_first = TRUE,
                            num_features = 10000, num_impostors = 500,
                            ...){

  # We tokenise the corpus, removing punctuation first if user desires
  tokenised_words <- authorverifyr::tokenise(data, remove_punct_first = remove_punct_first)
  # tokenised_chars <- authorverifyr::tokenise(data, what = "character",
  #                                            remove_punct_first = remove_punct_first)

  # In the paper the authors select n tokens, default is 500
  n_tokens <- authorverifyr::select_n_tokens(tokenised_words)

  # Create X and Y
  x <- character_n_grams(n_tokens$x)
  y <- character_n_grams(n_tokens$y)

  # Get the top features
  top_feats <- sort(quanteda::featfreq(rbind(x, y)), decreasing = TRUE) |>
    utils::head(num_features) |>
    names()

  # Return the tf*idf for x and y
  x <- quanteda::dfm_tfidf(x)
  y <- quanteda::dfm_tfidf(y)

  # If small number of features we convert the amount of max features
  if(num_features > length(top_feats)){
    num_features <- length(top_feats)
  }

  # Inititalise the dataframe
  result <- data.frame(matrix(ncol = 7, nrow = 0))
  col_names_result <- c("id", "author_x", "author_y", "same_author",
                        "score_x", "score_y", "score")
  colnames(result) <- col_names_result

  # Repeat this process for every document in X and Y.
  for(i in 1:nrow(x)){

    print(paste0("Document: ", i))
    chosen_x <- x[i,]
    chosen_y <- y[i,]

    # Get id and same_author details.
    chosen_id <- quanteda::docvars(chosen_x, 'id')
    author_x <- quanteda::docvars(chosen_x, 'author')
    author_y <- quanteda::docvars(chosen_y, 'author')
    same_author <- ifelse(author_x == author_y, TRUE, FALSE)

    # Create the impostor dataframes making sure to remove the current id
    impostor_x <- quanteda::dfm_subset(x, x$id != chosen_id) |>
      quanteda::dfm_sample(num_impostors)

    impostor_y <- quanteda::dfm_subset(y, y$id != chosen_id) |>
      quanteda::dfm_sample(num_impostors)

    # Initialise the scores
    score_x <- 0
    score_y <- 0

    for(k in 1:100){

      # Sample half the features, we do this for both X and Y
      x_samp_features <- sample(top_feats, num_features / 2)
      y_samp_features <- sample(top_feats, num_features / 2)

      # Match the dfm's for the chosen X
      chosen_x1 <- as.vector(quanteda::dfm_match(chosen_x, x_samp_features))
      chosen_x2 <- as.vector(quanteda::dfm_match(chosen_x, y_samp_features))

      # And the same for Y
      chosen_y1 <- as.vector(quanteda::dfm_match(chosen_y, x_samp_features))
      chosen_y2 <- as.vector(quanteda::dfm_match(chosen_y, y_samp_features))

      # Comparison variables, x_y is X vs Y impostors, y_x is Y vs X impostors
      score_x_y <- as.numeric(lsa::cosine(chosen_x1, chosen_y1))
      score_y_x <- as.numeric(lsa::cosine(chosen_x2, chosen_y2))

      # If all tf*idf in a vector is 0 then cosine would give NA
      if(is.na(score_x_y)){score_x_y <- 0}
      if(is.na(score_y_x)){score_y_x <- 0}

      # Match the impostor documents
      y_imp <- quanteda::dfm_match(impostor_y, x_samp_features)
      x_imp <- quanteda::dfm_match(impostor_x, y_samp_features)

      score_x_imp <- c()
      score_y_imp <- c()
      # Now we find the scores for every impostor

      for(m in 1:num_impostors){

        # Calculate the score between x, y and impostors
        score_xm <- as.numeric(lsa::cosine(chosen_x1, as.vector(y_imp[m,])))
        score_ym <- as.numeric(lsa::cosine(chosen_y2, as.vector(x_imp[m,])))

        # Again if all features are 0 will produce NA
        if(is.na(score_xm)){score_xm <- 0}
        if(is.na(score_ym)){score_ym <- 0}

        score_x_imp <- append(score_x_imp, score_xm)
        score_y_imp <- append(score_y_imp, score_ym)
      }

      # Once impostor similarities calculated, compare the scores
      if(all(score_x_imp <= score_x_y)){
        score_x <- round(score_x + 0.01, 2)}

      if(all(score_y_imp <= score_y_x)){
        score_y <- round(score_y + 0.01, 2)}

    }

    # We need the average between score_x and score_y
    score <- (score_x + score_y) / 2

    result_new <- cbind(chosen_id, author_x, author_y, same_author,
                        score_x, score_y, score)

    result <- rbind(result, result_new)

  }

  return(result)

}
