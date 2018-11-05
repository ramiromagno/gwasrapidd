context("test-utils")

#
## quote
#
test_that("Test add_quotes2", {
  expect_identical(add_quotes("a"), "\"a\"")
  expect_identical(add_quotes(""), "\"\"")
})

#
## is_links
#

test_that("Test is_links", {
  expect_identical(is_links("_links"), TRUE)
  expect_identical(
    is_links(c("_links", "links", "A_links", "_linksB")),
    c(TRUE, FALSE, FALSE, TRUE))
  expect_identical(is_links(
    c("_links", "links", "A_links", "_linksB", NA_character_)),
    c(TRUE, FALSE, FALSE, TRUE, NA))
  expect_identical(is_links(
    c("_links", "links", "A_links", "_linksB", NA_character_), convert_NA_to_FALSE = TRUE),
  c(TRUE, FALSE, FALSE, TRUE, FALSE))
  expect_error(is_links(1), "str argument must be a character vector.")
  expect_error(is_links(character()), "str contains no values, it must contain at least one string.")
})

#
## drop_links
#

test_that("Test drop_links", {
  vec1 <- 1:10
  vec2 <- LETTERS[vec1]
  lst_nolinks <- list(x = vec1, y = vec2)
  lst_wlinks <- list(x = vec1, y = vec2, "_links" = vec2)
  expect_error(drop_links(vec1), "lst must be of type list.")

  empty_lst <- list()
  warn_msg <- "Input list is empty. Nothing to do. Returning the input list as is."
  expect_warning(drop_links(empty_lst), warn_msg)

  lst_nonames <- list(vec1, vec2)
  warn_msg2 <- "Input list has no names. Nothing to do. Returning the input list as is."
  expect_warning(drop_links(lst_nonames), warn_msg2)

  expect_identical(drop_links(lst_nolinks), lst_nolinks)
  expect_identical(drop_links(lst_wlinks), lst_nolinks)
})

#
## recursive_apply
#

test_that("Test recursive_apply", {
  lst1 <- list(a = 1, b = 2, c = 3)
  df1 <- tibble::tibble(a = 1, b = 2, c = 3)
  df2 <- data.frame(a = 1, b = 2, c = 3)
  expect_identical(recursive_apply(lst1, identity), lst1)
  expect_identical(recursive_apply(df1, identity), df1)
  expect_identical(recursive_apply(df2, identity), tibble::as.tibble(df2))
  expect_identical(recursive_apply(lst1, function(x){x+1}), list(a = 2, b = 3, c = 4))
  expect_identical(recursive_apply(df1, function(x){x+1}), tibble::tibble(a = 2, b = 3, c = 4))
})

#
## null_to_na
#

test_that("Test null_to_na", {
  lst1 <- list(a = 1, b = 2, c = NULL)
  df1 <- tibble::tibble(a = 1, b = 2, c = list(NULL))
  expect_identical(null_to_na(lst1), list(a = 1, b = 2, c = NA_character_))
  expect_identical(null_to_na(df1), tibble::tibble(a = 1, b = 2, c = list(NA_character_)))
})
