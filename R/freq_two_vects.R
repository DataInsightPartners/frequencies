#' freq_two_vects
#'
#' freq_two_vects takes two columns from a data frame and returns a data frame containing frequency
#'    tables with counts and percentages of col2 within col1.
#'    It answers the question, what percent of col1 is col2.
#'
#'   As an example, if col1 was gender and col2 was ethnicity you would get the count and rate of
#'   males that are asian, african american, hispanic, etc.
#'
#'   If col1 was ethnicity and col2 was gender you would get the count and rate of asians that are
#'   male and female.
#'
#' @param df   a data frame
#' @param col1   a column from the data frame to be aggregated at the higher level.
#' @param col2   a column from the data frame to be aggregated within col1.
#' @param separate_tables a boolean that decises if you want all aggregations in one data frame
#'   or split apart so each element of col1 gets it's own table; the default is FALSE
#'
#' @return returns a list containing frequency tables split by col1_string with counts and rates of
#'         col2_string.
#' @export
#' @importFrom dplyr "%>%" as_data_frame group_by
#'
#' @examples
#' # Sample data frame to demo the freq_two_vects function.
#' df <- data.frame(gender = sample(c('m','f'), 200, replace = TRUE),
#'                  ethnicity = sample(c('african american', 'asian', 'caucasian',
#'                                    'hispanic', 'other'),
#'                                    200, replace = TRUE),
#'                  stringsAsFactors = FALSE)
#'
#' freq_two_vects(df, gender, ethnicity, FALSE)
#' gender_by_ethnicity <- freq_two_vects(df, gender, ethnicity, TRUE)
#' gender_by_ethnicity$m
#' freq_two_vects(df, gender, ethnicity, TRUE)$m
#' freq_two_vects(df, ethnicity, gender, FALSE)
#' ethnicity_by_gender <- freq_two_vects(df, ethnicity, gender, TRUE)
#' ethnicity_by_gender$asian

freq_two_vects <- function(df, col1, col2, separate_tables = FALSE){

  # To prevent NOTE from R CMD check 'no visible binding for global variable'
  get.col1. = . = n = NULL

  # Check that the df exists and is of type list.
  if (!exists(deparse(substitute(df)))) return(stop('Data frame does not exist. Do not put df in quotes.'))
  if (typeof(df) != 'list') {
    type <- typeof(df)
    err_message <- paste('df was of type "', type, '". Type "list" needed.', sep = '')
    return(stop(err_message))
  }

  # Check that the columns exist.
  col1 <- deparse(substitute(col1))
  col2 <- deparse(substitute(col2))
  if (!(col1 %in% names(df))) return(stop(paste(col1, 'not in df. Do not put col1 in quotes.')))
  if (!(col2 %in% names(df))) return(stop(paste(col2, 'not in df. Do not put col2 in quotes.')))

  if (!is.atomic(df[[col1]])) return(stop(paste(col1, 'is not an atomic vector.')))
  if (!is.atomic(df[[col2]])) return(stop(paste(col2, 'is not an atomic vector.')))

  # Check that separate_tables is logical
  if (!is.logical(separate_tables) & !(separate_tables %in% c(0,1)))
    return(stop('separate_tables was not logical. Need TRUE or FALSE'))


  result <- with(df, xtabs(~get(col1) + get(col2))) %>%
    dplyr::as_data_frame(.) %>%
    dplyr::group_by(get.col1.) %>%
    dplyr::mutate(Percentage = round(n * 100 / sum(n), 1)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(get.col1.)

  # Converts the result vector class to match original vector class. This prevents
  #  numbers and dates from being converted to characters so they can be sorted as intended.
  tryCatch({
    result[,1] <- eval(parse(text = paste('as.', class(df[[col1]])[1], '(result$get.col1.)', sep = '')))
  }, error = function(e) {
    print('Unable to convert col1 to original class.')
  })
  tryCatch({
    result[,2] <- eval(parse(text = paste('as.', class(df[[col2]])[1], '(result$get.col2.)', sep = '')))
  }, error = function(e) {
    print('Unable to convert col2 to original class.')
  })



  colnames(result) <- c(col1, col2, 'Count', 'Percentage')
  result <- dplyr::arrange_(result, col1, 'desc(Count)')

  if (separate_tables) {
    result <- split(result, result[[col1]])
    return(result)
  } else {
    return(result)
  }
}
