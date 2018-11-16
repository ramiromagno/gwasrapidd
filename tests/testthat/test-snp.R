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

test_that("Test as_genomic_range: positions greater than max_end_position", {
  expect_error(as_genomic_range("X", 1000000000L, 1000000001L), "All start positions must be lesser than 999999999, these are not: 1000000000.")
  expect_error(as_genomic_range("X", 999999999L, 1000000000L), "All end positions must be lesser than 999999999, these are not: 1000000000.")
})

test_that("Test as_genomic_range: start pos", {
  expect_error(as_genomic_range("X", 1, 2L), "start needs to be an integer vector, append an \"L\" to the number.")
  expect_error(as_genomic_range("X", "1", 2L), "start needs to be an integer vector.")
  expect_error(as_genomic_range("X", integer(0), 2L), "start is empty, must have at least one start position.")
})

test_that("Test as_genomic_range: end pos", {
  expect_error(as_genomic_range("X", 1L, 2), "end needs to be an integer vector, append an \"L\" to the number.")
  expect_error(as_genomic_range("X", 1L, "2"), "end needs to be an integer vector.")
  expect_error(as_genomic_range("X", 1L, integer(0)), "end is empty, must have at least one end position.")
})


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
  expect_error(as_genomic_range("X", 1L, 0L), "All end positions must be greater than 1, these are not: 0.")
})

test_that("Test as_genomic_range: chromosome names.", {
  expect_error(as_genomic_range(1, 1L, 2L), "chr needs to a character vector.")
  expect_error(as_genomic_range(1L, 1L, 2L), "chr needs to a character vector.")
  expect_error(as_genomic_range(FALSE, 1L, 2L), "chr needs to a character vector.")

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

test_that("filter_genomic_location_by_chr_name: more than one location.", {
  tbl_input <- tibble::tibble(
    chromosomeName = c("1", "1"),
    chromosomePosition = c(2570077L, 2570077L),
    region.name = c("1p36.32", "1p36.32"),
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
    filter_genomic_location_by_chr_name(tbl_input, warnings = FALSE),
    tbl_output)
  expect_warning(filter_genomic_location_by_chr_name(tbl_input, warnings = TRUE), "Filtering of genomic locations did not result in one unique location!\nPicking the first, ad hoc.")
})

#
## filter_genomic_location_by_chr_name
#

test_that("filter_genomic_location_by_chr_name: check input type", {
  expect_error(filter_genomic_location_by_chr_name(list()),
               "df needs to be a dataframe.")
  df <- tibble::tibble("chromosomeName" = character(),
                       "chromosomePosition" = integer(),
                       "region.name" = character(),
                       "_links.snps.href" = character())

  error_msg <- "df must contain all of the following variables:\nchromosomeName, chromosomePosition, region.name and _links.snps.href."
  # All columns except the first
  expect_error(filter_genomic_location_by_chr_name(df[, -1]),
               error_msg)

  # All columns except the second
  expect_error(filter_genomic_location_by_chr_name(df[, -2]),
               error_msg)

  # All columns except the third
  expect_error(filter_genomic_location_by_chr_name(df[, -3]),
               error_msg)

  # All columns except the fourth
  expect_error(filter_genomic_location_by_chr_name(df[, -4]),
               error_msg)

  expect_silent(df2 <- filter_genomic_location_by_chr_name(df, warnings = FALSE))
  df3 <- tibble::tibble("chromosomeName" = NA_character_,
                        "chromosomePosition" = NA_integer_,
                        "region.name" = NA_character_,
                        "_links.snps.href" = NA_character_)
  expect_identical(df2, df3)
  expect_warning(filter_genomic_location_by_chr_name(df, warnings = TRUE),
                "The dataframe df is empty. Filling in NAs...")
})


with_mock_api({
  test_that("filter_genomic_location_by_chr_name: typical use case", {
    snp_lst <- request("/singleNucleotidePolymorphisms/rs10910092")
    expect_is(snp_lst, "list")
    # Two locations are returned: 1:2570077 and CHR_HSCHR1_1_CTG3:2570077
    # (nrow == 2)
    expect_identical(nrow(snp_lst$content$locations), 2L)
    expect_identical(snp_lst$content$locations$chromosomeName,
                     c("1", "CHR_HSCHR1_1_CTG3"))
    location_tbl <- filter_genomic_location_by_chr_name(snp_lst$content$locations)
    # After filtering, only one location is returned (nrow == 1)
    expect_identical(nrow(location_tbl), 1L)
    expect_identical(location_tbl$chromosomeName, "1")
  })
})

with_mock_api({
  test_that("filter_genomic_location_by_chr_name: post filtering resulting in empty dataframe", {

    df_NA <- tibble::tibble("chromosomeName" = NA_character_,
                          "chromosomePosition" = NA_integer_,
                          "region.name" = NA_character_,
                          "_links.snps.href" = NA_character_)

    snp_lst <- request("/singleNucleotidePolymorphisms/rs10910092")

    # Removing the first row, i.e. the genomic location in chromosome 1
    # leaving only the genomic location CHR_HSCHR1_1_CTG3 that should
    # now be removed by filter_genomic_location_by_chr_name().
    location_tbl <- snp_lst$content$locations[-1, ]
     expect_warning(
       location_tbl2 <- filter_genomic_location_by_chr_name(location_tbl),
       "Filtering of genomic locations resulted in an empty dataframe!")
    # After filtering, the dataframe should now have one row only of NAs
    expect_identical(location_tbl2, df_NA)

     expect_silent(
       location_tbl2 <-
         filter_genomic_location_by_chr_name(location_tbl,
                                             warnings = FALSE))

    expect_identical(location_tbl2, df_NA)
  })
})
