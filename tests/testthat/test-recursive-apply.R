context("test-recursive-apply")

test_that('recursive_apply: simple cases', {
  expect_identical(recursive_apply(list(), identity), list())
  expect_identical(recursive_apply(list(a = 1), identity), list(a = 1))
  expect_identical(recursive_apply(list(a = 1, b = 2), identity), list(a = 1, b = 2))
  expect_identical(recursive_apply(list(a = NA_character_, b = 2), identity), list(a = NA_character_, b = 2))
  expect_identical(recursive_apply(list(a = NULL, b = 2), identity), list(a = NULL, b = 2))
})

test_that('recursive_apply: simple cases w/ nesting', {
  expect_identical(recursive_apply(list(a = list(a = 1)), identity), list(a = list(a = 1)))
  expect_identical(recursive_apply(list(a = 1, b = list(a = 1, b = 2)), identity), list(a = 1, b = list(a = 1, b = 2)))
  expect_identical(recursive_apply(list(
    a = NA_character_, b = list(a = NA_character_, b = 2)
  ), identity),
  list(a = NA_character_, b = list(a = NA_character_, b = 2)))
})

test_that('recursive_apply: simple cases, fn = length', {
  expect_identical(recursive_apply(list(), length), list())
  expect_identical(recursive_apply(list(a = 1), length), list(a = 1L))
  expect_identical(recursive_apply(list(a = 1, b = 2), length), list(a = 1L, b = 1L))
  expect_identical(recursive_apply(list(a = NA_character_, b = 2), length), list(a = 1L, b = 1L))
  expect_identical(recursive_apply(list(a = NULL, b = character()), length), list(a = 0L, b = 0L))
})

test_that('recursive_apply: simple cases w/ nesting, fn = length', {
  expect_identical(recursive_apply(list(a = list(a = 1)), length), list(a = list(a = 1L)))
  expect_identical(recursive_apply(list(a = 1, b = list(a = 1, b = 2)), length), list(a = 1L, b = list(a = 1L, b = 1L)))
  expect_identical(recursive_apply(list(
    a = NA_character_, b = list(a = NA_character_, b = c(1,1))
  ), length),
  list(a = 1L, b = list(a = 1L, b = 2L)))
})
