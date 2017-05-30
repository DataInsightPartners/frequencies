library(frequencies)
context("freq_table")

test_that("data exists",{
  expect_error(freq_table('nonexistant', 'nonexistant'))
})

