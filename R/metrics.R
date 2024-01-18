#--------------------------------------------------------------------------------#
#------------------------------------METRICS-------------------------------------#
#--------------------------------------------------------------------------------#

#' Get available performance metrics
#'
#' @param loc Location of the
#'
#' @return A vector containing the available metrics
available_metrics_trial <- function(loc = "metrics.R"){

  location <- paste0(system.file(package = "authorverifyr"), "/R/", loc)

  temp_env <- new.env()

  # Source the script into the temporary environment
  source(location, local = temp_env)

  # Get all objects from the temporary environment
  all_objects <- ls(temp_env)

  # Extract function names
  function_names <- sapply(all_objects, function(obj_name) {
    obj <- get(obj_name, envir = temp_env)
    if (is.function(obj)) return(obj_name)
    else return(NULL)
  })

  # Turn into vector and filter out this function.
  function_names <- as.vector(function_names[!sapply(function_names, is.null)])
  function_names <- function_names[!function_names == "available_metrics"]

  return(function_names)
}

#' Get available performance metrics for use with other functions
#'
#' @return A vector containing the available metrics
#' @export
available_metrics <- function(){

  # Update this manually when new metric added or removed.
  function_names <- c('TP', 'TN', 'FP', 'FN',
                      'accuracy', 'precision', 'recall', 'specificity',
                      'F1', 'F1PAN', 'F0.5u', 'cat1')

  return(function_names)
}

# True Positive
TP <- function(predicted, actual){
  tp <- sum(predicted == 1 & actual == 1)

  return(tp)
}

# True Negative
TN <- function(predicted, actual){
  tn <- sum(predicted == 0 & actual == 0)

  return(tn)
}

# False Positive
FP <- function(predicted, actual){
  fp <- sum(predicted == 1 & actual == 0)

  return(fp)
}

# False Negative
FN <- function(predicted, actual){
  fn <- sum(predicted == 0 & actual == 1)

  return(fn)
}

# Accuracy
accuracy <- function(predicted, actual){
  result <- (TP(predicted, actual) + TN(predicted, actual)) /
    (TP(predicted, actual) + FP(predicted, actual) + FN(predicted, actual) + TN(predicted, actual))

  return(result)
}

# Precision
precision <- function(predicted, actual){
  result <- TP(predicted, actual) / (TP(predicted, actual) + FP(predicted, actual))

  return(result)
}

# Recall
recall <- function(predicted, actual){
  result <- TP(predicted, actual) / (TP(predicted, actual) + FN(predicted, actual))

  return(result)
}

# Specificity
specificity <- function(predicted, actual){
  result <- TN(predicted, actual) / (TN(predicted, actual) + FP(predicted, actual))
}

# F1 Score
F1 <- function(predicted, actual){
  result <- 2 * (precision(predicted, actual) * recall(predicted, actual)) /
    (precision(predicted, actual) + recall(predicted, actual))

  return(result)
}

# F1 PAN Score - different precision and recall
F1PAN <- function(predicted, actual, n = length(predicted)){
  unanswered <- sum(predicted == 0.5)
  result <- 2 * ((TP(predicted, actual) + TN(predicted, actual)) /
                   (2*n - unanswered))
}

# F0.5u Score
F0.5u <- function(predicted, actual){
  positive <- TP(predicted, actual) + FP(predicted, actual)
  unanswered <- sum(predicted == 0.5)

  numerator <- (1 + 0.5^2) * TP(predicted, actual)
  denominator <- numerator + (0.5^2 * (FN(predicted, actual) + unanswered) + FP(predicted, actual))

  result <- numerator / denominator

  return(result)
}

# c@1
cat1 <- function(predicted, actual, n = length(predicted)){
  correct <- TN(predicted, actual) + TP(predicted, actual)
  unanswered <- sum(predicted == 0.5)

  result <- (1/n) * (correct + (correct/n) * unanswered)

  return(result)
}
