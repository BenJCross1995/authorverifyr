#--------------------------------------------------------------------------------#
#------------------------------------RESULTS-------------------------------------#
#--------------------------------------------------------------------------------#

#' Create a results table grouped by a threshold
#'
#' @param data A dataframe to loop over various thresholds
#' @param start_threshold Start of the threshold options
#' @param end_threshold End of threshold sequence
#' @param increment The increments of the threshold
#' @param metrics The selection of metrics to use
#' @param unanswered_if_equal Logical value depicting whether to include unanswered option if value is the same as threshold.
#'
#' @return A result dataframe.
#' @export
result_threshold_df <- function(data, start_threshold = 0, end_threshold = 1, increment = 0.01,
                      metrics = c('precision', 'recall'), unanswered_if_equal = TRUE) {

  # Create sequence of threshold values
  threshold_vec <- seq(start_threshold, end_threshold, increment)

  # Specific PAN metrics
  if(length(metrics) == 1 && tolower(metrics) == "pan"){
    metrics <- c('precision', 'recall', 'F1')
  }

  # Return the related functions
  metrics_funs <- lapply(metrics, match.fun)

  result_with_threshold <- data.frame()

  # Loop through the threshold values and bind the tables together
  for(thresh in threshold_vec){

    # If unanswered is an option then if score equal to threshold considered unanswered else >=
    if(unanswered_if_equal == TRUE){

      # Convert the score depending on if the score is greater than the threshold etc.
      result <- data |>
        dplyr::mutate(threshold = thresh,
                      prediction = dplyr::case_when(
                        threshold < score ~ 1,
                        threshold == score ~ 0.5,
                        threshold > score ~ 0,
                        TRUE ~ NA_real_))


    } else {

      # If the user doesn't want the uncertain predictions measured.
      result <- data |>
        dplyr::mutate(threshold = thresh,
                      prediction = dplyr::case_when(
                        threshold <= score ~ 1,
                        threshold > score ~ 0,
                        TRUE ~ NA_real_))

    }

    # Convert to a logical value
    result$same_author <- as.logical(result$same_author)

    # Calculate the user-selected metrics based on prediction and same author
    result <- result |>
      dplyr::group_by(result$threshold) |>
      dplyr::do({
        metrics_results <- sapply(metrics_funs, function(f) f(result$prediction, result$same_author))
        stats::setNames(data.frame(t(metrics_results)), metrics)
      }) |>
      dplyr::rename('threshold' = 'result$threshold')

    # Bind the results together
    result_with_threshold <- rbind(result_with_threshold, result)
  }

  return(result_with_threshold)
}
