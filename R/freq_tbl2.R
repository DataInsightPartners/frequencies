#' freq_tbl2
#'
#' freq_tbl2 takes two columns from a data frame and returns a list containing frequency tables
#'    with counts and percentages of col2 within col1.
#'    It answers the question, what percent of col1 is col2.
#'
#'   For example, if col1 was gender and col2 was ethnicity you would get the count of rate of
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
#' @importFrom dplyr "%>%"
#'
#' @examples
#' df <- data.frame(gender = sample(c('m','f'), 200, replace = TRUE),
#'                  ethnicity = sample(c('african american', 'asian', 'caucasian', 'hispanic', 'other'),
#'                                    200, replace = TRUE),
#'                  stringsAsFactors = FALSE)
#' freq_tbl2(df, gender, ethnicity, FALSE)
#' gender_by_ethnicity <- freq_tbl2(df, gender, ethnicity, TRUE)
#' gender_by_ethnicity$m
#' freq_tbl2(df, gender, ethnicity, TRUE)$m
#' freq_tbl2(df, ethnicity, gender, FALSE)
#' ethnicity_by_gender <- freq_tbl2(df, ethnicity, gender, TRUE)
#' ethnicity_by_gender$asian

freq_tbl2 <- function(df, col1, col2, separate_tables = FALSE){
  col1 <- deparse(substitute(col1))
  col2 <- deparse(substitute(col2))

  result <- with(df, xtabs(~get(col1) + get(col2))) %>%
    as_data_frame(.) %>%
    group_by(get.col1.) %>%
    dplyr::mutate(Percentage = round(n * 100 / sum(n), 1)) %>%
    dplyr::arrange(get.col1.)

  if(separate_tables) {
    result <- split(result, result$get.col1.)

    for (n in 1:length(result)) {
      colnames(result[[n]]) <- c(col1, col2, 'Count', 'Percentage')
    }
    return(result)
  } else {
    colnames(result) <- c(col1, col2, 'Count', 'Percentage')

    return(result)
  }
}
