context("test-missing")

#
## recode_to_chr_na
#
test_that("recode_to_chr_na: typical use case", {
  expect_identical(recode_to_chr_na('foo'), 'foo')
  expect_identical(recode_to_chr_na('NR'), NA_character_)
  expect_identical(recode_to_chr_na('nr'), NA_character_)
  expect_identical(recode_to_chr_na('NA'), NA_character_)
  expect_identical(recode_to_chr_na('na'), NA_character_)
  expect_identical(recode_to_chr_na(''), NA_character_)
  expect_identical(recode_to_chr_na(' '), NA_character_)
  expect_identical(recode_to_chr_na('  '), NA_character_)
  expect_identical(recode_to_chr_na('', recode_empty_string = FALSE), '')
  expect_identical(recode_to_chr_na(' ', recode_empty_string = FALSE), ' ')
  expect_identical(recode_to_chr_na('  ', recode_empty_string = FALSE), '  ')
  expect_identical(recode_to_chr_na(character(), recode_empty_string = FALSE), character())
  expect_identical(recode_to_chr_na(character(), recode_empty_string = TRUE), character())
})

test_that("recode_to_chr_na: exceptions", {
  expect_error(recode_to_chr_na(1L), 'chr_vec is expected to be a character!')
  expect_error(recode_to_chr_na('foo', from = 1L), 'from is expected to be a character!')
})

#
## recode_missing
#
test_that("recode_missing: NULL", {
  expect_identical(recode_missing(NULL, type = 'chr'), NA_character_)
  expect_identical(recode_missing(NULL, type = 'int'), NA_integer_)
  expect_identical(recode_missing(NULL, type = 'dbl'), NA_real_)
  expect_identical(recode_missing(NULL, type = 'lgl'), NA)
})
test_that("recode_missing: input chr typical use case", {
  missing_vec <- c('nr', 'NR', 'NA', 'na', '', ' ', '  ', NA_character_)
  expect_identical(recode_missing(missing_vec), rep(NA_character_, 8L))
  expect_identical(recode_missing(c('', ' ', '  '), recode_empty_string = FALSE), c('', ' ', '  '))
  expect_identical(recode_missing(c('foo', 'bar')), c('foo', 'bar'))

  expect_identical(recode_missing(missing_vec, type = 'int'), rep(NA_integer_, 8L))
  expect_identical(recode_missing(c('', ' ', '  '), type = 'int', recode_empty_string = FALSE),
                   c(NA_integer_, NA_integer_, NA_integer_))
  expect_identical(recode_missing(c('foo', 'bar'), type = 'int'), c(NA_integer_, NA_integer_))

  expect_identical(recode_missing(missing_vec, type = 'dbl'), rep(NA_real_, 8L))
  expect_identical(recode_missing(c('', ' ', '  '), type = 'dbl', recode_empty_string = FALSE),
                   c(NA_real_, NA_real_, NA_real_))
  expect_identical(recode_missing(c('foo', 'bar'), type = 'dbl'), c(NA_real_, NA_real_))

  expect_identical(recode_missing(missing_vec, type = 'lgl'), rep(NA, 8L))
  expect_identical(recode_missing(c('', ' ', '  '), type = 'lgl', recode_empty_string = FALSE), c(NA, NA, NA))
  expect_identical(recode_missing(c('foo', 'bar'), type = 'lgl'), c(NA, NA))

  expect_identical(recode_missing(character(), type = 'int'), integer())
  expect_identical(recode_missing(character(), type = 'dbl'), double())
  expect_identical(recode_missing(character(), type = 'lgl'), logical())
  expect_identical(recode_missing(character(), type = 'chr'), character())
})
test_that("recode_missing: input int typical use case", {
  vec <- c(1L, 2L, NA_integer_)
  expect_identical(recode_missing(vec, type = 'int'), vec)
  expect_identical(recode_missing(vec, type = 'dbl'), c(1, 2, NA_real_))
  expect_identical(recode_missing(vec, type = 'lgl'), c(TRUE, TRUE, NA))
  expect_identical(recode_missing(vec, type = 'chr'), c('1', '2', NA_character_))

  expect_identical(recode_missing(integer(), type = 'int'), integer())
  expect_identical(recode_missing(integer(), type = 'dbl'), double())
  expect_identical(recode_missing(integer(), type = 'lgl'), logical())
  expect_identical(recode_missing(integer(), type = 'chr'), character())
})
test_that("recode_missing: input dbl typical use case", {
  vec <- c(1.0, 3.14, NA_real_)
  expect_identical(recode_missing(vec, type = 'int'), c(1L, 3L, NA_integer_))
  expect_identical(recode_missing(vec, type = 'dbl'), vec)
  expect_identical(recode_missing(vec, type = 'lgl'), c(TRUE, TRUE, NA))
  expect_identical(recode_missing(vec, type = 'chr'), c('1', '3.14', NA_character_))

  expect_identical(recode_missing(double(), type = 'int'), integer())
  expect_identical(recode_missing(double(), type = 'dbl'), double())
  expect_identical(recode_missing(double(), type = 'lgl'), logical())
  expect_identical(recode_missing(double(), type = 'chr'), character())
})
test_that("recode_missing: input lgl typical use case", {
  vec <- c(TRUE, FALSE, NA)
  expect_identical(recode_missing(vec, type = 'int'), c(1L, 0L, NA_integer_))
  expect_identical(recode_missing(vec, type = 'dbl'), c(1, 0, NA_real_))
  expect_identical(recode_missing(vec, type = 'lgl'), vec)
  expect_identical(recode_missing(vec, type = 'chr'), c('TRUE', 'FALSE', NA_character_))

  expect_identical(recode_missing(logical(), type = 'int'), integer())
  expect_identical(recode_missing(logical(), type = 'dbl'), double())
  expect_identical(recode_missing(logical(), type = 'lgl'), logical())
  expect_identical(recode_missing(logical(), type = 'chr'), character())
})

test_that("recode_missing: exceptions", {
  expect_error(recode_missing(list()), 'x must be NULL, character, logical, integer or double.')
  expect_error(recode_missing('foo', type = 'wth?'),
               'type must be of one of these types: chr, dbl, int or lgl.')
  expect_error(recode_missing('foo', from = 1L), 'from must be character.')
})
