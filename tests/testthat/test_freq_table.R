library(frequencies)
context("freq_table")
set.seed(1)
tbl <<- data.frame(numbers = sample(1:10, 200, replace = TRUE),
                  letters = sample(letters, 200, replace = TRUE),
                  dates = sample(seq(as.Date('1999/10/01'), as.Date('2000/01/01'), by = "day"), 200, replace = TRUE),
                  logicals = sample(c(TRUE, FALSE), 200, replace = TRUE),
                  stringsAsFactors = FALSE)

test_that("data exists",{
  expect_error(freq_table(nonexistant, 'numbers'), "data_frame_string needs to be in quotes")
  expect_error(freq_table('nonexistant', 'numbers'), "Data frame referenced does not exist")
  expect_error(freq_table('tbl', nonexistant), "column_string needs to be in quotes")
  expect_error(freq_table('tbl', 'nonexistant'), 'Column contained no data / does not exist')
})

test_that('numbers aggregations are correct', {

  expect_equal(freq_table('tbl', 'numbers', FALSE, FALSE)$Count,
                   c(12, 17, 22, 23, 24, 19, 21, 27, 20 ,15))
  expect_equal(freq_table('tbl', 'numbers', FALSE, TRUE)$Count,
                   c('12', '17', '22', '23', '24', '19', '21', '27', '20' ,'15', '200'))
  expect_equal(freq_table('tbl', 'numbers', TRUE, TRUE)$Count,
                   c('27', '24', '23', '22', '21', '20', '19', '17', '15' ,'12', '200'))
  expect_equal(freq_table('tbl', 'numbers', TRUE, FALSE)$Count,
               c(27, 24, 23, 22, 21, 20, 19, 17, 15 ,12))
})

test_that('letters aggregations are correct', {
  expect_equal(freq_table('tbl', 'letters', FALSE, FALSE)$Count[1:10],
               c(1, 8, 10, 12, 14, 7, 11, 7, 10, 12))
})

