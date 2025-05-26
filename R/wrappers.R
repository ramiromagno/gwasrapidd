# Wrappers around imported functions to make mocking easier with:
# - testthat::with_mocked_bindings
# - testthat::local_mocked_bindings
#
browse_url <- function(...) utils::browseURL(...)
is_online <- function(...) pingr::is_online(...)
ping_port <- function(...) pingr::ping_port(...)
status_code <- function(...) httr::status_code(...)
skip <- function(...) testthat::skip(...)
readline <- function(...) base::readline(...)
str_detect <- function(...) stringr::str_detect(...)
