context("test-utils")

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
  # Keep an eye on https://github.com/r-lib/testthat/issues/599
  #empty_lst <- list()
  #warn_msg <- "Input list is empty. Nothing to do. Returning the input list as is."
  #expect_warning(drop_links(empty_lst), warn_msg)
  #lst_nonames <- list(vec1, vec2)
  #warn_msg2 <- "Input list has no names. Nothing to do. Returning the input list as is."
  #expect_warning(drop_links(lst_nonames), warn_msg2)
  expect_identical(drop_links(lst_nolinks), lst_nolinks)
  expect_identical(drop_links(lst_wlinks), lst_nolinks)
})

