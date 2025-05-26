context("test-post-studies")

test_that("open_in_pubmed: exceptions", {
  expect_error(open_in_pubmed(pubmed_id = FALSE), "pubmed_id must be a vector of numbers.")
  expect_error(open_in_pubmed(pubmed_id = -123), "These are not valid PubMed IDs: -123.")
})

test_that("open_in_pubmed: typical usage", {
  with_mocked_bindings(
    browse_url = function(...)
      TRUE,
    code =
      {
        expect_true(open_in_pubmed(pubmed_id = 123))
        expect_true(open_in_pubmed(pubmed_id = 123L))
        expect_true(open_in_pubmed(pubmed_id = '123'))
      }
  )
})
