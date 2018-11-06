context("test-request")

#
# request()
#

with_mock_api({
  test_that("request: bare", {
    response <- request()
    expect_is(response, "list")
    top_elements <- names(response)
    expect_identical(top_elements, c("response_code", "content"))
  })
})

with_mock_api({
  test_that("request: non-existant resource", {
    # Requesting with warnings = TRUE
    wrn1 <- "The request for https://www.ebi.ac.uk/gwas/rest/api/fernandopessoa did not completed successfully."
    expect_warning(response <- request("/fernandopessoa"), wrn1)
    expect_is(response, "list")
    top_elements <- names(response)
    expect_identical(top_elements, c("response_code", "content"))
    expect_identical(response$response_code, 404L)
    expect_is(response$content, "NULL")
    # Requesting with warnings = FALSE
    expect_warning(response <- request("/fernandopessoa", warnings = FALSE), NA)
    expect_is(response, "list")
    top_elements <- names(response)
    expect_identical(top_elements, c("response_code", "content"))
    expect_identical(response$response_code, 404L)
    expect_is(response$content, "NULL")
  })
})

with_mock_api({
  test_that("request: try a resource that does not return JSON", {
    # Use here httpbin.org for this test
    expect_error(
      request(base_url = "https://httpbin.org", resource_url = "/html"),
    "Response did not return JSON")
  })
})

with_mock_api({
  test_that("request: verbose", {
    msg1 <- "Base URL: https://www.ebi.ac.uk/gwas/rest/api."
    msg2 <- "Requesting resource: https://www.ebi.ac.uk/gwas/rest/api/."
    msg3 <- "Using the user agent: gwasrapidd: GWAS R API Data Download."
    msg4 <- "Response code: 200."
    msg5 <- "Response content type is \"application/json\"."
    expect_message(response <- request(verbose = TRUE), msg1)
    expect_message(response <- request(verbose = TRUE), msg2)
    expect_message(response <- request(verbose = TRUE), msg3)
    expect_message(response <- request(verbose = TRUE), msg4)
    expect_message(response <- request(verbose = TRUE), msg5)
    expect_is(response, "list")
    top_elements <- names(response)
    expect_identical(top_elements, c("response_code", "content"))
  })
})

with_mock_api({
  test_that("request: warnings", {
    expect_warning(response <- request(warnings = TRUE), NA)
    expect_is(response, "list")
    top_elements <- names(response)
    expect_identical(top_elements, c("response_code", "content"))
  })
})
