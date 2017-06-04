library(frequencies)
context("freq_table")
set.seed(1)
tbl <<- data.frame(numbers = sample(1:10, 200, replace = TRUE),
                  letters = sample(letters, 200, replace = TRUE),
                  dates = sample(seq(as.Date('1999/10/01'), as.Date('2000/01/01'), by = "day"),
                                 200, replace = TRUE),
                  logicals = sample(c(TRUE, FALSE), 200, replace = TRUE),
                  stringsAsFactors = FALSE)

test_that("data exists",{
  expect_error(freq_table(nonexistant, 'numbers'), "data_frame_string needs to be in quotes")
  expect_error(freq_table('nonexistant', 'numbers'), "Data frame referenced does not exist")
  expect_error(freq_table('tbl', nonexistant), "column_string needs to be in quotes")
  expect_error(freq_table('tbl', 'nonexistant'), 'Column contained no data / does not exist')
})

checkNumbersFF <- data.frame(numbers =    c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L),
                             Count =      c(12L, 17L, 22L, 23L, 24L, 19L, 21L, 27L, 20L, 15L),
                             Percentage = c(6.0, 8.5, 11.0, 11.5, 12.0, 9.5, 10.5, 13.5, 10.0, 7.5),
                             Cum. =       c(6.0, 14.5, 25.5, 37.0, 49.0 , 58.5, 69.0, 82.5, 92.5,
                                            100.0))

checkNumbersFT <- data.frame(numbers =    c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10',
                                            'Total'),
                             Count =      c('12', '17', '22', '23', '24', '19', '21', '27', '20',
                                            '15', '200'),
                             Percentage = c('6', '8.5', '11', '11.5', '12', '9.5', '10.5', '13.5',
                                            '10', '7.5', '100'),
                             Cum. =       c('6', '14.5', '25.5', '37', '49' , '58.5', '69', '82.5',
                                            '92.5', '100', '100'),
                             stringsAsFactors = FALSE)

checkNumbersTT <- data.frame(numbers =    c('8', '5', '4', '3', '7', '9', '6', '2', '10', '1',
                                            'Total'),
                             Count =      c('27', '24', '23', '22', '21', '20', '19', '17', '15',
                                            '12', '200'),
                             Percentage = c('13.5', '12', '11.5', '11', '10.5', '10', '9.5', '8.5',
                                            '7.5', '6', '100'),
                             Cum. =       c('13.5', '25.5', '37', '48', '58.5' , '68.5', '78',
                                            '86.5', '94', '100', '100'),
                             stringsAsFactors = FALSE)

checkNumbersTF <- data.frame(numbers =    c(8L, 5L, 4L, 3L, 7L, 9L, 6L, 2L, 10L, 1L),
                             Count =      c(27L, 24L, 23L, 22L, 21L, 20L, 19L, 17L, 15L, 12L),
                             Percentage = c(13.5, 12.0, 11.5, 11.0, 10.5, 10.0, 9.5, 8.5, 7.5, 6.0),
                             Cum. =       c(13.5, 25.5, 37.0, 48.0, 58.5 , 68.5, 78.0, 86.5, 94.0,
                                            100.0))

test_that('numbers aggregations are correct', {
  expect_equal(freq_table('tbl', 'numbers', FALSE, FALSE), checkNumbersFF)
  expect_equal(freq_table('tbl', 'numbers', FALSE, TRUE),  checkNumbersFT)
  expect_equal(freq_table('tbl', 'numbers', TRUE, TRUE),   checkNumbersTT)
  expect_equal(freq_table('tbl', 'numbers', TRUE, FALSE),  checkNumbersTF)

})

checkLettersFF <- data.frame(letters =    c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'),
                             Count =      c(1L, 8L, 10L, 12L, 14L, 7L, 11L, 7L, 10L, 12L),
                             Percentage = c(0.5, 4.0, 5.0, 6.0, 7.0, 3.5, 5.5, 3.5, 5.0, 6.0),
                             Cum. =       c(0.5, 4.5, 9.5, 15.5, 22.5, 26.0, 31.5, 35.0, 40.0, 46.0),
                             stringsAsFactors = FALSE)

checkLettersFT <- data.frame(letters =    c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
                                            'Total'),
                             Count =      c('1', '8', '10', '12', '14', '7', '11', '7', '10', '12',
                                            '200'),
                             Percentage = c('0.5', '4', '5', '6', '7', '3.5', '5.5', '3.5', '5',
                                            '6', '100'),
                             Cum. =       c('0.5', '4.5', '9.5', '15.5', '22.5', '26', '31.5', '35',
                                            '40', '46' ,'100'),
                             stringsAsFactors = FALSE)

checkLettersTT <- data.frame(letters =    c('e', 'd', 'j', 'm', 'y', 'g', 'c', 'i', 'z', 'b',
                                            'Total'),
                             Count =      c('14', '12', '12', '12', '12', '11', '10', '10', '10',
                                            '8', '200'),
                             Percentage = c('7', '6', '6', '6', '6', '5.5', '5', '5' , '5', '4',
                                            '100'),
                             Cum. =       c('7', '13', '19', '25', '31', '36.5', '41.5', '46.5',
                                            '51.5', '55.5' ,'100'),
                             stringsAsFactors = FALSE)

checkLettersTF <- data.frame(letters =    c('e', 'd', 'j', 'm', 'y', 'g', 'c', 'i', 'z', 'b'),
                             Count =      c(14L, 12L, 12L, 12L, 12L, 11L, 10L, 10L, 10L, 8L),
                             Percentage = c(7.0,  6.0,  6.0,  6.0,  6.0,  5.5,  5.0,  5.0 , 5.0,
                                            4.0),
                             Cum. =       c(7.0, 13.0, 19.0, 25.0, 31.0, 36.5, 41.5, 46.5, 51.5,
                                            55.5),
                             stringsAsFactors = FALSE)

test_that('letters aggregations are correct', {
  expect_equal(freq_table('tbl', 'letters', FALSE, FALSE)[1:10,],      checkLettersFF)
  expect_equal(freq_table('tbl', 'letters', FALSE, TRUE)[c(1:10,27),], checkLettersFT)
  expect_equal(freq_table('tbl', 'letters', TRUE, TRUE)[c(1:10,27),],  checkLettersTT)
  expect_equal(freq_table('tbl', 'letters', TRUE, FALSE)[1:10,],       checkLettersTF)
})


checkDatesFF <- data.frame(dates = as.Date(c('1999-10-01', '1999-10-02', '1999-10-03', '1999-10-04',
                                             '1999-10-05', '1999-10-06', '1999-10-07', '1999-10-08',
                                             '1999-10-09', '1999-10-10')),
                           Count =      c(3L, 2L, 2L, 2L, 3L, 2L, 3L, 2L, 2L, 1L),
                           Percentage = c(1.5, 1.0, 1.0, 1.0, 1.5, 1.0, 1.5, 1.0, 1.0, 0.5),
                           Cum. =       c(1.5, 2.5, 3.5, 4.5, 6.0, 7.0, 8.5, 9.5, 10.5, 11.0),
                           stringsAsFactors = FALSE)

checkDatesFT <- data.frame(dates =    c('1999-10-01', '1999-10-02', '1999-10-03', '1999-10-04',
                                        '1999-10-05', '1999-10-06', '1999-10-07', '1999-10-08',
                                        '1999-10-09', '1999-10-10', 'Total'),
                           Count =      c('3', '2', '2', '2', '3', '2', '3', '2', '2', '1', '200'),
                           Percentage = c('1.5', '1', '1', '1', '1.5', '1', '1.5', '1', '1', '0.5',
                                          '100'),
                           Cum. =       c('1.5', '2.5', '3.5', '4.5', '6', '7', '8.5', '9.5',
                                          '10.5', '11', '100'),
                           stringsAsFactors = FALSE)

checkDatesTT <- data.frame(dates =    c('1999-11-30', '1999-12-27', '1999-11-14', '1999-11-16',
                                        '1999-12-01', '1999-12-04', '1999-12-28', '1999-10-16',
                                        '1999-11-01', '1999-11-04' ,'Total'),
                           Count =      c('6', '6', '5', '5', '5', '5', '5', '4', '4', '4', '200'),
                           Percentage = c('3', '3', '2.5', '2.5', '2.5', '2.5', '2.5', '2', '2', '2',
                                          '100'),
                           Cum. =       c('3', '6', '8.5', '11', '13.5', '16', '18.5', '20.5',
                                          '22.5', '24.5', '100'),
                           stringsAsFactors = FALSE)

checkDatesTF <- data.frame(dates =    as.Date(c('1999-11-30', '1999-12-27', '1999-11-14',
                                                '1999-11-16', '1999-12-01', '1999-12-04',
                                                '1999-12-28', '1999-10-16', '1999-11-01',
                                                '1999-11-04')),
                           Count =      c(6L, 6L, 5L, 5L, 5L, 5L, 5L, 4L, 4L, 4L),
                           Percentage = c(3.0, 3.0, 2.5, 2.5, 2.5, 2.5, 2.5, 2.0, 2.0, 2.0),
                           Cum. =       c(3.0, 6.0, 8.5, 11.0, 13.5, 16.0, 18.5, 20.5, 22.5, 24.5),
                           stringsAsFactors = FALSE)

test_that('dates aggregations are correct', {
  expect_equal(freq_table('tbl', 'dates', FALSE, FALSE)[1:10,],      checkDatesFF)
  expect_equal(freq_table('tbl', 'dates', FALSE, TRUE)[c(1:10,85),], checkDatesFT)
  expect_equal(freq_table('tbl', 'dates', TRUE, TRUE)[c(1:10,85),],  checkDatesTT)
  expect_equal(freq_table('tbl', 'dates', TRUE, FALSE)[1:10,],       checkDatesTF)
})


checkLogicalsFF <- data.frame(logicals = c(FALSE, TRUE),
                              Count =      c(103L, 97L),
                              Percentage = c(51.5, 48.5),
                              Cum. =       c(51.5, 100.0))

checkLogicalsFT <- data.frame(logicals = c('FALSE',  'TRUE',  'Total'),
                              Count =      c('103',  '97',    '200'),
                              Percentage = c('51.5', '48.5',  '100'),
                              Cum. =       c('51.5', '100', '100'),
                              stringsAsFactors = FALSE)

checkLogicalsTT <- data.frame(logicals = c('FALSE',  'TRUE',  'Total'),
                              Count =      c('103',  '97',    '200'),
                              Percentage = c('51.5', '48.5',  '100'),
                              Cum. =       c('51.5', '100', '100'),
                              stringsAsFactors = FALSE)

checkLogicalsTF <- data.frame(logicals = c(FALSE, TRUE),
                              Count =      c(103L, 97L),
                              Percentage = c(51.5, 48.5),
                              Cum. =       c(51.5, 100.0))

test_that('logicals aggregations are correct', {
  expect_equal(freq_table('tbl', 'logicals', FALSE, FALSE), checkLogicalsFF)
  expect_equal(freq_table('tbl', 'logicals', FALSE, TRUE),  checkLogicalsFT)
  expect_equal(freq_table('tbl', 'logicals', TRUE, TRUE),   checkLogicalsTT)
  expect_equal(freq_table('tbl', 'logicals', TRUE, FALSE),  checkLogicalsTF)
})

