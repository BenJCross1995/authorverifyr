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

  result_with_threshold <- data.frame()

  # Loop through the threshold values and bind the tables together
  for(thresh in threshold_vec){

    # If unanswered is an option then if score equal to threshold considered unanswered else >=
    if(unanswered_if_equal == TRUE){
      result <- data |>
        dplyr::mutate(threshold = thresh,
                      prediction = dplyr::case_when(
                        threshold < score ~ 1,
                        threshold == score ~ 0.5,
                        threshold > score ~ 0,
                        TRUE ~ NA_real_))
    } else {
      result <- data |>
        dplyr::mutate(threshold = thresh,
                      prediction = dplyr::case_when(
                        threshold <= score ~ 1,
                        threshold > score ~ 0,
                        TRUE ~ NA_real_))
    }
    # Bind the results together
    result_with_threshold <- rbind(result_with_threshold, result)
  }

  # Convert to logical for calcs
  result_with_threshold$same_author <- as.logical(result_with_threshold$same_author)

  # Specific PAN metrics
  if(length(metrics) == 1 && tolower(metrics) == "pan"){
    metrics <- c('precision', 'recall', 'F1')
  }

  # Return the related functions
  metrics_funs <- lapply(metrics, match.fun)

  # Summarise based on prediction and same_author columns and call the resulting columns
  # by the name of the functions in metrics
  summarised_results <- result_with_threshold |>
    dplyr::group_by_("threshold") |>
    dplyr::do({
      metrics_results <- sapply(metrics_funs, function(f) f(result_with_threshold$prediction,
                                                            result_with_threshold$same_author))
      stats::setNames(data.frame(t(metrics_results)), metrics)
    })

  return(summarised_results)
}
