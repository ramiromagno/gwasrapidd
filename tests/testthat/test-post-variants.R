context("test-post-variants")

test_that("open_in_dbsnp", {
  with_mock(
    `browseURL` = function(...) TRUE,
    expect_true(open_in_dbsnp('blah')),
    expect_error(open_in_dbsnp(42L), "variant_id must be a character vector.")
  )
})

test_that("open_in_gtex", {
  with_mock(
    `browseURL` = function(...) TRUE,
    expect_true(open_in_gtex('blah')),
    expect_error(open_in_gtex(42L), "variant_id must be a character vector.")
  )
})
