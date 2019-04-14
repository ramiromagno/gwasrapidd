context("test-utils")

test_that('equal_length: normal use', {
  expect_identical(equal_length(list(1L, 1:2)), list(c(1L, NA_integer_), 1:2))
  expect_identical(equal_length(list()), list())
})

test_that('equal_length: exceptions', {
  expect_error(equal_length(1L), 'lst_of_vectors must be a list!')
  expect_error(equal_length(list(list())), 'One or more elements of lst_of_vectors is not an atomic vector.')
})
