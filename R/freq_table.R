#' freq_table
#'
#' freq_table returns a frequency table with counts and percentages of values
#'   from the provided data frame and column.
#'
#' @param data_frame_string a string of the name of a data frame
#' @param column_string a string of the column to be aggregated
#'
#' @return a tibble containing the counts and percentages of each value from the provided data
#' @export
#' @importFrom dplyr "%>%"
#'
#' @examples
#' tbl <- data_frame(numbers = sample(1:10, 200, replace = TRUE),
#'                   letters = sample(letters, 200, replace = TRUE))
#' freq_table('tbl', 'numbers')
#' View(freq_table('tbl', 'letters'))
freq_table <- function(data_frame_string, column_string) {

  result <- dplyr::count_(get(data_frame_string), column_string) %>%
              dplyr::mutate(total = sum(n)) %>%
              dplyr::group_by_(column_string) %>%
              dplyr::mutate(Percentage = round(n * 100 / total, 1)) %>%
              dplyr::select_(column_string,
                      Count = 'n',
                      'Percentage')
  return(result)
 }





