context("test-post-variants")

test_that("open_in_dbsnp", {
  local_mocked_bindings(browse_url = function(...) TRUE)
  expect_true(open_in_dbsnp('blah'))
  expect_error(open_in_dbsnp(42L), "variant_id must be a character vector.")
})

test_that("open_in_gtex", {
  local_mocked_bindings(browse_url = function(...) TRUE)
  expect_true(open_in_gtex('blah'))
  expect_error(open_in_gtex(42L), "variant_id must be a character vector.")
})
