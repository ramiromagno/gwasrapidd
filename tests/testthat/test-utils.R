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
