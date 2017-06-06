library(frequencies)
context("freq_tbl2")
set.seed(1)
tbl <- data.frame(numbers = sample(1:3, 200, replace = TRUE),
                   letters = sample(letters[1:3], 200, replace = TRUE),
                   dates = sample(seq(as.Date('1999/10/01'), as.Date('1999/10/03'), by = "day"),
                                  200, replace = TRUE),
                   logicals = sample(c(TRUE, FALSE), 200, replace = TRUE),
                   stringsAsFactors = FALSE)

test_that("data exists",{
  expect_error(freq_tbl2(nonexistant, nonexistant), "Data frame does not exist. Do not put df in quotes.")
  expect_error(freq_tbl2(tbl, nonexistant), "nonexistant not in df. Do not put col1 in quotes.")
  expect_error(freq_tbl2('tbl', nonexistant), "Data frame does not exist. Do not put df in quotes.")
  expect_error(freq_tbl2(tbl, 'numbers'), '"numbers" not in df. Do not put col1 in quotes.')
  expect_error(freq_tbl2(data.frame, nonexistant), 'df was of type "closure". Type "list" needed.')
})

