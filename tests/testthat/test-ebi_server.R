context("test-ebi_server")

# https://www.mango-solutions.com/blog/testing-without-the-internet-using-mock-functions

#
# is_ebi_reachable()
#

test_that("is_ebi_reachable: should return FALSE if computer cannot ping.", {
  with_mock(`pingr::is_online` = function(...) FALSE,
            expect_message(status <- is_ebi_reachable(), "It seems you are not online.")
            )
  expect_identical(status, FALSE)

  with_mock(`pingr::is_online` = function(...) TRUE,
            `pingr::ping_port` = function(...) NA,
            expect_message(is_ebi_reachable(chatty = TRUE),
                           "Am I online? Yes.",
                           "www.ebi.ac.uk is not replying to ping requests on port 443.",
                           fixed = TRUE),
            expect_message(is_ebi_reachable(chatty = TRUE),
                           "www.ebi.ac.uk is not replying to ping requests on port 443.",
                           fixed = TRUE),
            expect_identical(is_ebi_reachable(chatty = TRUE), FALSE)
  )

  with_mock(`pingr::is_online` = function(...) TRUE,
            `pingr::ping_port` = function(...) 10.0, # time in miliseconds
            expect_message(is_ebi_reachable(chatty = TRUE),
                           "Am I online? Yes.",
                           fixed = TRUE),

            expect_message(is_ebi_reachable(chatty = TRUE),
                           "Is there a DNS service running? Yes.",
                           fixed = TRUE),

            expect_message(is_ebi_reachable(chatty = TRUE),
                           "Connected to the internet? Yes.",
                           fixed = TRUE),

            expect_message(is_ebi_reachable(chatty = TRUE),
                           "Ping'ing www.ebi.ac.uk on port 443...",
                           fixed = TRUE),

            expect_message(is_ebi_reachable(chatty = TRUE),
                           "www.ebi.ac.uk successfully replied in 10 ms.",
                           fixed = TRUE),

            expect_identical(is_ebi_reachable(), TRUE),
            expect_identical(is_ebi_reachable(chatty = TRUE), TRUE)
  )
})

test_that("is_ebi_reachable: should return FALSE if there is no DNS.", {
  with_mock(`pingr::is_online` = function(...) "nodns",
            expect_message(status <- is_ebi_reachable(),
                           "It seems there is no Domain Name Server (DNS) service running.",
                           fixed = TRUE)
  )
  expect_identical(status, FALSE)
})
