context("test-study")

test_that("Test is_study_accession convert_NA_to_FALSE option.", {
  expect_equal(is_study_accession(NA_character_), as.logical(NA))
  expect_equal(is_study_accession(c(NA_character_, NA_character_)), as.logical(c(NA, NA)))
  expect_equal(is_study_accession(NA_character_, convert_NA_to_FALSE = TRUE), FALSE)
  expect_equal(is_study_accession(c("GCST000000", NA_character_), convert_NA_to_FALSE = TRUE), c(TRUE, FALSE))
})

test_that("Test is_study_accession errors.", {
  expect_error(is_study_accession(1), "str argument must be a character vector.")
  expect_error(is_study_accession(character(0)), "str contains no values, it must contain at least one string.")
})

test_that("Test is_study_accession.", {
  expect_equal(is_study_accession("GCST123456"),   TRUE)
  expect_equal(is_study_accession("GCST000000"),   TRUE)
  expect_equal(is_study_accession("GCST999999"),   TRUE)
  expect_equal(is_study_accession("GCST000001"),   TRUE)
  expect_equal(is_study_accession(" GCST000000"),  FALSE)
  expect_equal(is_study_accession("GCST000000 "),  FALSE)
  expect_equal(is_study_accession("GCST 000000 "), FALSE)
  expect_equal(is_study_accession("000000"),       FALSE)
  expect_equal(is_study_accession("CST000001"),    FALSE)
  expect_equal(is_study_accession("GCS000001"),    FALSE)
  expect_equal(is_study_accession("GCST12345"),    FALSE)
})
