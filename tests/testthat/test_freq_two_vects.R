library(frequencies)
context("freq_two_vects")
set.seed(1)
tbl <<- data.frame(numbers = sample(1:3, 200, replace = TRUE),
                   letters = sample(letters[1:3], 200, replace = TRUE),
                   dates = sample(seq(as.Date('1999/10/01'), as.Date('1999/10/03'), by = "day"),
                                  200, replace = TRUE),
                   logicals = sample(c(TRUE, FALSE), 200, replace = TRUE),
                   stringsAsFactors = FALSE)

test_that("data exists",{
  expect_error(freq_two_vects(nonexistant, nonexistant), "Data frame does not exist. Do not put df in quotes.")
  expect_error(freq_two_vects(tbl, nonexistant), "nonexistant not in df. Do not put col1 in quotes.")
  expect_error(freq_two_vects('tbl', nonexistant), "Data frame does not exist. Do not put df in quotes.")
  expect_error(freq_two_vects(tbl, 'numbers'), '"numbers" not in df. Do not put col1 in quotes.')
  expect_error(freq_two_vects(data.frame, nonexistant), 'df was of type "closure". Type "list" needed.')
})

check_numbers_letters_f <- data.frame(numbers = c(1L, 1L, 1L, 2L, 2L),
                                      letters = c('a', 'b', 'c', 'a', 'c'),
                                      Count   = c(25L, 21L, 10L, 28L, 27L),
                                      Percentage = c(44.6, 37.5, 17.9, 35.9, 34.6),
                                      stringsAsFactors = FALSE)

check_numbers_letters_t1 <- data.frame(numbers = c(1L, 1L, 1L),
                                       letters = c('a', 'b', 'c'),
                                       Count   = c(25L, 21L, 10L),
                                       Percentage = c(44.6, 37.5, 17.9),
                                       stringsAsFactors = FALSE)

check_numbers_letters_t2 <- data.frame(numbers = c(2L, 2L, 2L),
                                       letters = c('a', 'c', 'b'),
                                       Count   = c(28L, 27L, 23L),
                                       Percentage = c(35.9, 34.6, 29.5),
                                       stringsAsFactors = FALSE)

check_letters_numbers_f <- data.frame(letters    = c('a', 'a', 'a', 'b', 'b'),
                                      numbers    = c(2L, 1L, 3L, 2L, 1L),
                                      Count      = c(28L, 25L, 25L, 23L, 21L),
                                      Percentage = c(35.9, 32.1, 32.1, 35.4, 32.3),
                                      stringsAsFactors = FALSE)

test_that('numbers and letters aggregations are correct', {
  expect_equal(freq_two_vects(tbl, numbers, letters, FALSE)[1:5,], check_numbers_letters_f)
  expect_equal(freq_two_vects(tbl, numbers, letters, TRUE)[[1]],   check_numbers_letters_t1)
  expect_equal(freq_two_vects(tbl, numbers, letters, TRUE)[[2]],   check_numbers_letters_t2)
  expect_equal(freq_two_vects(tbl, letters, numbers, FALSE)[1:5,],  check_letters_numbers_f)
})

check_dates_logicals_f <- data.frame(dates = as.Date(c('1999-10-01', '1999-10-01', '1999-10-02',
                                                       '1999-10-02', '1999-10-03')),
                                     logicals = c(TRUE, FALSE, FALSE, TRUE, FALSE),
                                     Count = c(32L, 21L, 41L, 36L, 41L),
                                     Percentage = c(60.4, 39.6, 53.2, 46.8, 58.6),
                                     stringsAsFactors = FALSE)

check_dates_logicals_t1 <- data.frame(dates = as.Date(c('1999-10-01', '1999-10-01')),
                                      logicals = c(TRUE, FALSE),
                                      Count = c(32L, 21L),
                                      Percentage = c(60.4, 39.6),
                                      stringsAsFactors = FALSE)

check_dates_logicals_t2 <- data.frame(dates = as.Date(c('1999-10-02', '1999-10-02')),
                                     logicals = c(FALSE, TRUE),
                                     Count = c(41L, 36L),
                                     Percentage = c(53.2, 46.8),
                                     stringsAsFactors = FALSE)

check_logicals_dates_f <- data.frame(logicals = c(FALSE, FALSE, FALSE, TRUE, TRUE),
                                     dates = as.Date(c('1999-10-02', '1999-10-03', '1999-10-01',
                                                       '1999-10-02', '1999-10-01')),
                                     Count = c(41L, 41L, 21L, 36L, 32L),
                                     Percentage = c(39.8, 39.8, 20.4, 37.1, 33.0),
                                     stringsAsFactors = FALSE)

test_that('dates and logicals aggregations are correct', {
  expect_equal(freq_two_vects(tbl, dates, logicals, FALSE)[1:5,], check_dates_logicals_f)
  expect_equal(freq_two_vects(tbl, dates, logicals, TRUE)[[1]], check_dates_logicals_t1)
  expect_equal(freq_two_vects(tbl, dates, logicals, TRUE)[[2]], check_dates_logicals_t2)
  expect_equal(freq_two_vects(tbl, logicals, dates, FALSE)[1:5,], check_logicals_dates_f)
})


