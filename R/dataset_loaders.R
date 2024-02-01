
#' Generic lapply function to use parallel if possible
#'
#' @param X The list item
#' @param FUN The function to call
#' @param ... Any additional function parameters
#'
#' @return A list of blogs
lapply_parallel <- function(X, FUN, ...) {
  # Check if 'future.apply' is available
  if (requireNamespace("future.apply", quietly = TRUE) && requireNamespace("future", quietly = TRUE)) {
    # Set up future plan without explicitly loading the 'future' package
    future::plan(future::multisession)

    # Use 'future_lapply' from 'future.apply'
    return(future.apply::future_lapply(X, FUN, ...))
  } else {
    # Fallback to the base 'lapply'
    return(base::lapply(X, FUN, ...))
  }
}

#' Return the blogger.com text from a filepath
#'
#' @param row The row of the dataframe
#'
#' @return The date of the blogpost and the post itself
extract_blog_text <- function(row){

  tryCatch({
    # Attempt XML parsing
    xml_data <- xml2::read_xml(row$filepath)
  }, error = function(e) {
    # Fallback to HTML parsing in case of XML parsing error
    xml_data <<- xml2::read_html(row$filepath)
    return(xml_data)
  })

  # Extract the date and post
  dates <- xml2::xml_find_all(xml_data, ".//date")
  posts <- xml2::xml_find_all(xml_data, ".//post")

  # Extracting text from nodes
  date_text <- xml2::xml_text(dates)
  post_text <- xml2::xml_text(posts)

  # Create a temporary dataframe from the data
  temp_df <- data.frame(date = date_text,
                        post = trimws(gsub("\n|\t", "", post_text)))

  # Repeat the metadata for the number of new rows
  metadata_df <- row[rep(1, nrow(temp_df)), ]

  # Combine the rows
  combined_df <- dplyr::bind_cols(metadata_df, temp_df)

  return(combined_df)

}

#' Load Blogger.com Corpus
#'
#' @param loc The location of the blog corpus
#' @param convert_to_corpus Binary parameter convert to quanteda corpus TRUE/FALSE
#'
#' @return The data as a corpus or dataframe
#' @export
load_blog_corpus <- function(loc, convert_to_corpus = TRUE){

  blog_loc <- loc
  df <- data.frame(filepath = list.files(blog_loc))

  # Separate the filepath column into its parts.
  df <- df |>
    tidyr::separate(col = "filepath",
                    into = c("author_id", "gender", "age", "job", "starsign", "filetype"),
                    sep = "\\.",
                    remove = FALSE,  # Set to FALSE if you want to keep the original column
                    convert = TRUE)  |> # Converts to appropriate data types
    dplyr::mutate(filepath = paste0(blog_loc, "/", df$filepath))

  # Use lapply to apply the function over each row of the DataFrame
  list_of_dfs <- lapply_parallel(1:nrow(df), function(i) extract_blog_text(df[i, ]))

  # Combine all DataFrames into one
  final_df <- dplyr::bind_rows(list_of_dfs)

  # Add an id column - potentially remove
  final_df <- cbind('id' = seq(1, nrow(final_df)), final_df[,-c(1, 7)]) |>
    dplyr::mutate(date = as.Date(final_df$date, format = "%d,%B,%Y"))

  # Remove rownames so text not stored as Text1.1 etc.
  row.names(final_df) <- NULL

  # Convert it to a corpus if user requires
  if(convert_to_corpus == TRUE){
    final_df <- quanteda::corpus(final_df, text_field = "post")
  }

  return(final_df)

}
