#' Setup the environment to skip slow tests
#'
#' This function sets an environment variable \code{TEST_FAST} to 'true' which
#' is used to flag if a test should be skipped or not. If you place
#' \code{\link[gwasrapidd]{skip_if_testing_is_fast}} in a test then it will
#' check if \code{TEST_FAST} is 'true', if it is, then it will skip the next
#' expectations within a \code{test_that()} block.
#' Please note that this function is to be used in gwasrapidd development.
#'
#' @return Returns \code{NULL}. This function should be used for its side
#'   effect.
#' @keywords internal
set_testing_fast <- function() {
  Sys.setenv(TEST_FAST = "true")
}

#' Setup the environment to still run slow tests
#'
#' This function sets an environment variable \code{TEST_FAST} to 'false' which
#' is used to flag if a test should be skipped or not. If you place
#' \code{\link[gwasrapidd]{skip_if_testing_is_fast}} in a test then it will
#' check if \code{TEST_FAST} is 'true', if it is, then it will skip the next
#' expectations with a \code{test_that()} block. Please note that this function
#' is to be used in gwasrapidd development.
#'
#' @return Returns \code{NULL}. This function should be used for its side
#'   effect.
#'
#' @keywords internal
set_testing_slow <- function() {
  Sys.setenv(TEST_FAST = "false")
}

#' Skips a test if TEST_FAST is 'true'
#'
#' This function checks the value of \code{TEST_FAST}, if it is 'true' it skips
#' the test otherwise it still runs the following expectations. Please note that
#' this function is to be used in gwasrapidd development.
#'
#' @return Returns \code{TRUE} if the environment variable \code{'TEST_FAST'} is
#'   set to \code{'false'}, or does not return anything but triggers the side
#'   effect of skipping the next test (with testthat).
#'
#' @keywords internal
skip_if_testing_is_fast <- function()
{
  # If slow we return TRUE, so that the following expectations are run.
  # Note that both 'true' or simply '' (empty string) will lead to skipping.
  if (identical(Sys.getenv("TEST_FAST"), "false")) {
    return(invisible(TRUE))
  }
  # If fast, then we skip the next expectations.
  testthat::skip("Arriba, arriba! Andale, andale!")
}
