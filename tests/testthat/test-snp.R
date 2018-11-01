context("test-snp")

#
## is_genomic_range
#

test_that("Test is_genomic_range errors.", {
  expect_error(is_genomic_range(1), "str argument must be a character vector.")
  expect_error(
    is_genomic_range(character(0)),
    "str contains no values, it must contain at least one string."
  )
})

test_that("Test is_genomic_range.", {
  expect_equal(is_genomic_range(c("10:10-20")), TRUE)
  expect_equal(is_genomic_range(c("10:10-20", "X:1-2000", "Y:34-123")), c(TRUE, TRUE, TRUE))
  expect_equal(is_genomic_range(c("10:10-20", "X:1-2000", NA_character_)), c(TRUE, TRUE, NA))
  expect_equal(is_genomic_range(c("1:1-123456789", "1:1-0123456789")), c(TRUE, FALSE))
  expect_equal(is_genomic_range("chr1:1-10"), FALSE)
  expect_equal(is_genomic_range("1:10-5"), TRUE)
})

test_that("Test is_genomic_range convert_NA_to_FALSE option.", {
  expect_equal(is_genomic_range(c("10:10-20", "X:1-2000", NA_character_)), c(TRUE, TRUE, NA))
  expect_equal(is_genomic_range(
    c("10:10-20", "X:1-2000", NA_character_),
    convert_NA_to_FALSE = TRUE
  ),
  c(TRUE, TRUE, FALSE))
})

#
## as_genomic_range
#

test_that("Test as_genomic_range: starting_position_index option.", {
  expect_identical(as_genomic_range("1", 1L, 2L, starting_position_index = 0L),
                   "1:1-2")
  expect_identical(as_genomic_range("1", 1L, 2L, starting_position_index = 1L),
                   "1:1-2")
  expect_identical(as_genomic_range("X", 1L, 2L, starting_position_index = 0L),
                   "X:1-2")
  expect_identical(as_genomic_range("X", 1L, 2L, starting_position_index = 1L),
                   "X:1-2")
  expect_identical(as_genomic_range("X", 0L, 2L, starting_position_index = 0L),
                   "X:0-2")
  expect_error(
    as_genomic_range("1", 1L, 2L, starting_position_index = 2L),
    "starting_position_index must be either 0L or 1L."
  )
  expect_error(
    as_genomic_range("X", 1L, 2L, starting_position_index = -2L),
    "starting_position_index must be either 0L or 1L."
  )
  expect_identical(as_genomic_range("1", 0L, 2L, starting_position_index = 0L),
                   "1:0-2")
  expect_error(
    as_genomic_range("1", 0L, 2L, starting_position_index = 1L),
    "All start positions must be greater than 1, these are not: 0."
  )
  expect_error(
    as_genomic_range(c("X", "Y"), c(0L, -1L), c(2L, 2L), starting_position_index = 1L),
    "All start positions must be greater than 1, these are not: 0 and -1.",
    fixed = TRUE
  )
})

test_that("Test as_genomic_range: chromosome names.", {
  expect_error(
    as_genomic_range(character(0L), 1L, 2L),
    "chr is empty, must have at least one human chromosome name."
  )
  expect_error(as_genomic_range("", 1L, 2L),
               "The following are not human chromosome names: \"\".")
  tmp1_chr <- as.character(1:22)
  tmp1_start <- rep(1L, 22)
  tmp1_end <- rep(10L, 22)
  tmp1_gen_ranges <-
    sprintf("%s:%d-%d", tmp1_chr, tmp1_start, tmp1_end)
  expect_identical(as_genomic_range(tmp1_chr, tmp1_start, tmp1_end),
                   tmp1_gen_ranges)
  expect_identical(as_genomic_range("X", 1L, 2L), "X:1-2")
  expect_identical(as_genomic_range("Y", 1L, 2L), "Y:1-2")
  expect_error(as_genomic_range("x", 1L, 2L),
               "The following are not human chromosome names: \"x\".") # Because it's case sensitive
  expect_error(as_genomic_range("y", 1L, 2L),
               "The following are not human chromosome names: \"y\".") # Because it's case sensitive
  expect_error(as_genomic_range("Z", 1L, 2L),
               "The following are not human chromosome names: \"Z\".")
})

test_that("Test as_genomic_range: check that chr, start and end are of the same length.",
          {
            expect_error(
              as_genomic_range(c("X", "Y"), 10L, 20L),
              "chr, start and end vectors should be of same length: len(chr) = 2, len(start) = 1, and len(end) = 1.",
              fixed = TRUE
            )
            expect_error(
              as_genomic_range("X", c(10L, 15L), 20L),
              "chr, start and end vectors should be of same length: len(chr) = 1, len(start) = 2, and len(end) = 1.",
              fixed = TRUE
            )
            expect_error(
              as_genomic_range("X", 10L, c(10L, 20L)),
              "chr, start and end vectors should be of same length: len(chr) = 1, len(start) = 1, and len(end) = 2.",
              fixed = TRUE
            )
          })

test_that("Test as_genomic_range: start is less or equal than end.", {
  expect_error(
    as_genomic_range("X", 21L, 20L),
    "start positions cannot be larger than end positions: X:21-20.",
    fixed = TRUE
  )
})


#
## filter_genomic_location_by_chr_name
#
test_that("Test filter_genomic_location_by_chr_name.", {
  tbl_input <- tibble::tibble(
    chromosomeName = c("1", "CHR_HSCHR1_1_CTG3"),
    chromosomePosition = c(2570077L, 2570077L),
    region.name = c("1p36.32", NA_character_),
    `_links.snps.href` = c(
      "https://www.ebi.ac.uk/gwas/rest/api/singleNucleotidePolymorphisms/rs10910092",
      "https://www.ebi.ac.uk/gwas/rest/api/singleNucleotidePolymorphisms/rs10910092"
    )
  )
  tbl_output <- tibble::tibble(
    chromosomeName = c("1"),
    chromosomePosition = c(2570077L),
    region.name = c("1p36.32"),
    `_links.snps.href` = "https://www.ebi.ac.uk/gwas/rest/api/singleNucleotidePolymorphisms/rs10910092"
  )
  expect_identical(
    filter_genomic_location_by_chr_name(tbl_input),
    tbl_output)
})

#
## is_rs_id
#

test_that("Test is_rs_id errors.", {
  expect_error(is_rs_id(1), "str argument must be a character vector.")
  expect_error(
    is_rs_id(character(0)),
    "str contains no values, it must contain at least one string."
  )
})

test_that("Test is_rs_id", {
  expect_equal(is_rs_id(c("rs0001")), TRUE)
  expect_equal(is_rs_id(c("rs123", "rs0001", "rs09123")), c(TRUE, TRUE, TRUE))
  expect_equal(is_rs_id(c("rs123", "rs0001", NA_character_)), c(TRUE, TRUE, NA))
  expect_equal(is_rs_id(c("rs1", "rs")), c(TRUE, FALSE))
  expect_equal(is_rs_id(" rs123"), FALSE)
  expect_equal(is_rs_id("rs 12312"), FALSE)
  expect_equal(is_rs_id(
    c("rs123", "rs0001", NA_character_),
    convert_NA_to_FALSE = TRUE), c(TRUE, TRUE, FALSE))
})
