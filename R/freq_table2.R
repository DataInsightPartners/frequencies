#' freq_table2
#'
#' freq_table2 takes two columns from a data frame and returns a list containing frequency tables
#'    with counts and percentages of col2 within col1.
#'    It answers the question, what percent of col1 is col2.
#'
#'   For example, if col1 was gender and col2 was ethnicity you would get the count of rate of
#'   males that are asian, african american, hispanic, etc.
#'
#'   If col1 was ethnicity and col2 was gender you would get the count and rate of asians that are
#'   male and female.
#'
#' @param df_string a string of the name of a data frame
#' @param col1_string a string of the column name to be aggregated at the higher level.
#' @param col2_string a string of the column name to be aggregated within col1_string.
#'
#' @return returns a list containing frequency tables split by col1_string with counts and rates of
#'         col2_string.
#' @export
#' @importFrom dplyr "%>%"
#'
#' @examples
#' df <- data_frame(gender = sample(c('m','f'), 200, replace = TRUE),
#'                  ethnicity = sample(c('african american', 'asian', 'caucasian', 'hispanic', 'other'),
#'                                    200, replace = TRUE))
#' freq_table2('df', 'gender', 'ethnicity')
#' gender_by_ethnicity <- freq_table2('df', 'gender', 'ethnicity')
#' gender_by_ethnictiy$m
#' freq_table2('df', 'gender', 'ethnicity')$m
#' freq_table2('df', 'ethnicity', 'gender')
#' ethnicity_by_gender <- freq_table2('df', 'ethnicity', 'gender')
#' ethnicity_by_gender$asian
freq_table2 <- function(df_string, col1_string, col2_string){
  result <- with(get(df_string), xtabs(~get(col1_string) + get(col2_string))) %>%
    dplyr::as_data_frame(.) %>%
    dplyr::group_by(get.col1_string.) %>%
    dplyr::mutate(Percentage = round(n * 100 / sum(n), 1)) %>%
    dplyr::arrange(get.col1_string.) %>%
    split(.$get.col1_string.)

  for (n in 1:length(result)) {
    colnames(result[[n]]) <- c(col1_string, col2_string, 'Count', 'Percentage')
  }

  return(result)
 }


