context("test-utils")

test_that('equal_length: normal use', {
  expect_identical(equal_length(list(1L, 1:2)), list(c(1L, NA_integer_), 1:2))
  expect_identical(equal_length(list()), list())
})

test_that('equal_length: exceptions', {
  expect_error(equal_length(1L), 'lst_of_vectors must be a list!')
  expect_error(equal_length(list(list())), 'One or more elements of lst_of_vectors is not an atomic vector.')
})


test_that('empty_to_na: exceptions', {
  expect_error(empty_to_na(list()), 'x must be an atomic vector')
})

test_that('empty_to_na: non-empty input', {
  expect_equal(empty_to_na(c(3.14, 42)), c(3.14, 42))
  expect_equal(empty_to_na(1:5), 1:5)
  expect_equal(empty_to_na(letters[1:3]), letters[1:3])
  expect_equal(empty_to_na(c(TRUE, FALSE)), c(TRUE, FALSE))
})

test_that('empty_to_na: empty input', {
  expect_equal(empty_to_na(double()), NA_real_)
  expect_equal(empty_to_na(integer()), NA_integer_)
  expect_equal(empty_to_na(character()), NA_character_)
  expect_equal(empty_to_na(logical()), NA)
})


test_that('empty_to_na: dates', {
  my_dates <- c("2010-04-14-04-35-59", "2010-04-01-12-00-00")
  x <- lubridate::ymd_hms(my_dates)
  expect_equal(empty_to_na(x), x)
  empty_date <- lubridate::ymd_hms()
  y <- empty_to_na(empty_date)
  expect_equal(y, lubridate::ymd_hms(NA_real_))
})
