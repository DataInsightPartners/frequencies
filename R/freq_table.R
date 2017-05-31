#' freq_table
#'
#' freq_table returns a frequency table with counts and percentages of values
#'   from the provided data frame and column.
#'
#' @param data_frame_string   a string of the name of a data frame
#' @param column_string   a string of the column to be aggregated
#' @param sort_by_count   a boolean value that determines if the output will be sorted by count or name
#' @param total_row   a boolean value that determines if the output will have a summary row appended
#'
#' @return a data_frame containing the counts and percentages of each value from the provided data
#' @export
#' @importFrom dplyr "%>%"
#'
#' @examples
#' tbl <- data.frame(numbers = sample(1:10, 200, replace = TRUE),
#'   letters = sample(letters, 200, replace = TRUE),
#'   dates = sample(seq(as.Date('1999/10/01'), as.Date('2000/01/01'), by="day"), 200, replace = TRUE),
#'   logicals = sample(c(TRUE, FALSE), 200, replace = TRUE),
#'   stringsAsFactors = FALSE)
#'
#' freq_table('tbl', 'numbers')
#' View(freq_table('tbl', 'letters'))
#' View(freq_table('tbl', 'letters', sort_by_count = TRUE, total_row = FALSE))

freq_table <- function(data_frame_string, column_string, sort_by_count = FALSE, total_row = TRUE) {

  # Check validity of data frame argument. The argument needs to be a string and the data frame needs to exist.
  tryCatch({
    if (typeof(data_frame_string) != 'character') return(stop('data_frame_string needs to be in quotes'))
  }, error = function(e){
    stop('data_frame_string needs to be in quotes')
  }
  )
  #if (typeof(data_frame_string) != 'character') return(stop('data_frame_string needs to be in quotes'))
  if (!exists(data_frame_string))               return(stop('Data frame referenced does not exist.'))

  # Check validity of column string argument.
  # The argument needs to be a string, the column needs to contain data / exist, and the data needs
  # to be of data type logical, integer, double, or character.
  tryCatch({
    if (typeof(column_string) != 'character') return(stop('column_string needs to be in quotes'))
      }, error = function(e){
        stop('column_string needs to be in quotes')
      }
  )
  check <- get(data_frame_string)
  if (length(check[[column_string]]) == 0) return(stop('Column contained no data / does not exist.'))
  if (!(typeof(check[[column_string]]) %in% c('logical', 'integer', 'double','character'))) return(stop('Vector not of acceptable data type.'))



  if (!is.logical(sort_by_count)) sort_by_count <- TRUE
  sort_by <- ifelse(sort_by_count, 'desc(n)', column_string)

  result <- dplyr::count_(get(data_frame_string), column_string) %>%
              dplyr::mutate(total = sum(n)) %>%
              dplyr::group_by_(column_string) %>%
              dplyr::mutate(Percentage = round(n * 100 / total, 1)) %>%
              dplyr::ungroup() %>%
              dplyr::arrange_(sort_by) %>%
              dplyr::mutate(Cum. = cumsum(Percentage)) %>%
              dplyr::select_(column_string,
                      Count = 'n',
                      'Percentage',
                      'Cum.')


  if (!is.logical(total_row)) total_row <- TRUE
  if (total_row) {
    result[,1] <- lapply(result[,1], as.character)
    result <- rbind.data.frame(result,c('Total', sum(result$Count), 100, 100))
  }

  return(result)
 }





