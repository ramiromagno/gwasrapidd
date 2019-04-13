context("test-list_joins")

#
# lst_left_join()
#
test_that("lst_left_join: empty lists", {
  expect_identical(lst_left_join(list(), list()), list())
  expect_identical(lst_left_join(list(foo = 1), list()), list(foo = 1))
  expect_identical(lst_left_join(list(), list(foo = 1)), list(foo = 1))
})

test_that("lst_left_join: simple shallow case", {
  lst_x <- list(a = 'a1',
                b = 'b1',
                c = 'c1',
                d = 'd1')
  lst_y <- list(b = 'b2',
                c = 'c2',
                d = 'd2',
                e = 'e2')

  lst_z <-
    list(
      a = 'a1',
      b = c('b1', 'b2'),
      c = c('c1', 'c2'),
      d = c('d1', 'd2')
    )
  expect_identical(lst_left_join(lst_x, lst_y), lst_z)
})

#
# lst_right_join()
#
test_that("lst_right_join: empty lists", {
  expect_identical(lst_right_join(list(), list()), list())
  expect_identical(lst_right_join(list(foo = 1), list()), list(foo = 1))
  expect_identical(lst_right_join(list(), list(foo = 1)), list(foo = 1))
})

test_that("lst_right_join: simple shallow case", {
  lst_x <- list(a = 'a1',
                b = 'b1',
                c = 'c1',
                d = 'd1')
  lst_y <- list(b = 'b2',
                c = 'c2',
                d = 'd2',
                e = 'e2')

  lst_z <-
    list(
      b = c('b1', 'b2'),
      c = c('c1', 'c2'),
      d = c('d1', 'd2'),
      e = 'e2'
    )
  expect_identical(lst_right_join(lst_x, lst_y), lst_z)
})

#
# lst_inner_join()
#
test_that("lst_inner_join: empty lists", {
  expect_identical(lst_inner_join(list(), list()), list())
  expect_identical(lst_inner_join(list(foo = 1), list()), list())
  expect_identical(lst_inner_join(list(), list(foo = 1)), list())
})

test_that("lst_inner_join: simple shallow case", {
  lst_x <- list(a = 'a1',
                b = 'b1',
                c = 'c1',
                d = 'd1')
  lst_y <- list(b = 'b2',
                c = 'c2',
                d = 'd2',
                e = 'e2')

  lst_z <-
    list(
      b = c('b1', 'b2'),
      c = c('c1', 'c2'),
      d = c('d1', 'd2')
    )
  expect_identical(lst_inner_join(lst_x, lst_y), lst_z)
})

test_that("plst_left_join: simple shallow case", {
  lst_x <- list(a = 'a1',
                b = 'b1',
                c = 'c1',
                d = 'd1')
  lst_y <- list(b = 'b2',
                c = 'c2',
                d = 'd2',
                e = 'e2')
  lst_z <- list(c = 'c3',
                d = 'd3',
                e = 'e3',
                f = 'f3')

  lst_result <-
    list(
      a = 'a1',
      b = c('b1', 'b2'),
      c = c('c1', 'c2', 'c3'),
      d = c('d1', 'd2', 'd3')
    )
  expect_identical(plst_left_join(list(lst_x, lst_y, lst_z)), lst_result)
})

test_that("plst_right_join: simple shallow case", {
  lst_x <- list(a = 'a1',
                b = 'b1',
                c = 'c1',
                d = 'd1')
  lst_y <- list(b = 'b2',
                c = 'c2',
                d = 'd2',
                e = 'e2')
  lst_z <- list(c = 'c3',
                d = 'd3',
                e = 'e3',
                f = 'f3')

  lst_result <-
    list(
      c = c('c1', 'c2', 'c3'),
      d = c('d1', 'd2', 'd3'),
      e = c('e2', 'e3'),
      f = c('f3')
    )
  expect_identical(plst_right_join(list(lst_x, lst_y, lst_z)), lst_result)
})
