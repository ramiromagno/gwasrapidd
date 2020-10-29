context("test-parse-utils")

#
## extract_association_id
#

test_that("extract_association_id", {
  expect_identical(extract_association_id("https://www.ebi.ac.uk/gwas/rest/api/associations/16510553"), '16510553')
  expect_identical(extract_association_id("/associations/16510553"), '16510553')
  expect_identical(extract_association_id(c("/associations/16510553", "/associations/24534")), c('16510553', '24534'))
})

#
## allele_name
#

test_that("allele_name", {
  expect_identical(allele_name("rs7885458-G"), 'G')
  expect_identical(allele_name("rs2405942-A"), 'A')
  expect_identical(allele_name("rs192021326-T"), 'T')
  expect_identical(allele_name("rs4129148-C"), 'C')
  expect_identical(allele_name("rs4129148-CGCTAG"), 'CGCTAG')
  expect_identical(allele_name("rs4129148-X"), NA_character_)
  expect_identical(allele_name("rs4129148-"), NA_character_)
  expect_identical(allele_name("rs4129148"), NA_character_)
  expect_identical(allele_name("rs4129148-wtf-AGTA"), 'AGTA')
  expect_identical(allele_name("rs4129148-wtf-"), NA_character_)
  expect_identical(allele_name(""), NA_character_)
})


#
## variant_name
#
test_that("variant_name", {
  expect_identical(variant_name("rs7885458-G"), 'rs7885458')
  expect_identical(variant_name("rs2405942-A"), 'rs2405942')
  expect_identical(variant_name("rs192021326-T"), 'rs192021326')
  expect_identical(variant_name("rs4129148-C"), 'rs4129148')
  expect_identical(variant_name("rs4129148-CGCTAG"), 'rs4129148')
  expect_identical(variant_name("rs4129148-ACC-CGAG"), 'rs4129148-ACC')
  expect_identical(variant_name("rs4129148-X"), 'rs4129148-X')
  expect_identical(variant_name("rs4129148-"), 'rs4129148')
  expect_identical(variant_name("rs4129148"), 'rs4129148')
  expect_identical(variant_name("rs4129148-wtf-AGTA"), 'rs4129148-wtf')
  expect_identical(variant_name("rs4129148-wtf-"), 'rs4129148-wtf')
  expect_identical(variant_name(""), NA_character_)
})

#
## is_rs_id
#
test_that("is_rs_id", {
  expect_true(is_rs_id("rs123"))
  expect_identical(is_rs_id(c("rs123", "rs0001", "rs09123")), c(TRUE, TRUE, TRUE))
  expect_identical(is_rs_id(c("rs123", "rs0001", NA_character_)), c(TRUE, TRUE, FALSE))
  expect_identical(is_rs_id(c("rs123", "rs0001", NA_character_), convert_NA_to_FALSE = FALSE), c(TRUE, TRUE, NA))
  expect_error(is_rs_id(1L), "str argument must be a character vector.")
  expect_error(is_rs_id(character()), "str contains no values, it must contain at least one string.")
})

#
## is_association_id
#
test_that("is_association_id", {
  expect_true(is_association_id("12345"))
  expect_identical(is_association_id(c("2345", "345367", "2145098")), c(TRUE, TRUE, TRUE))
  expect_identical(is_association_id(c("132324", "12955", NA_character_)), c(TRUE, TRUE, FALSE))
  expect_identical(is_association_id(c("132654", "5", NA_character_), convert_NA_to_FALSE = FALSE), c(TRUE, TRUE, NA))
  expect_error(is_association_id(1L), "str argument must be a character vector.")
  expect_error(is_association_id(character()), "str contains no values, it must contain at least one string.")
})

#
## is_human_chromosome
#
test_that("is_human_chromosome", {
  expect_true(is_human_chromosome("1"))
  expect_identical(is_human_chromosome(as.character(1:22)), rep(TRUE, 22))
  expect_identical(is_human_chromosome(c("X", "Y")), c(TRUE, TRUE))
  expect_identical(is_human_chromosome(c("x", "y")), c(FALSE, FALSE))
  expect_identical(is_human_chromosome('MT', chromosomes = c(1:22, 'X', 'Y', 'MT')), TRUE)
  expect_identical(is_human_chromosome(c('X', NA_character_)), c(TRUE, FALSE))
  expect_identical(is_human_chromosome(c('X', NA_character_), convert_NA_to_FALSE = FALSE), c(TRUE, NA))
})

#
## is_efo_id
#
test_that("is_efo_id", {
  expect_true(is_efo_id("EFO_1234567"))
  expect_identical(is_efo_id(c("EFO_1234567", "EFO_0000042", "123456")), c(TRUE, TRUE, FALSE))
  expect_identical(is_efo_id(c("1234567", "EFO_1234567", NA_character_)), c(FALSE, TRUE, FALSE))
  expect_identical(is_efo_id(c("1234567", "EFO_1234567", NA_character_), convert_NA_to_FALSE = FALSE), c(FALSE, TRUE, NA))
  expect_error(is_efo_id(1L), "str argument must be a character vector.")
  expect_error(is_efo_id(character()), "str contains no values, it must contain at least one string.")
})


#
## is_efo_id2
#
test_that("is_efo_id2", {
  expect_true(is_efo_id2("EFO_1234567"))
  expect_identical(is_efo_id2(c("EFO_1234567", "EFO_0000042", "123456")), c(TRUE, TRUE, TRUE))
  expect_identical(is_efo_id2(c("1234567", "EFO_1234567", NA_character_)), c(TRUE, TRUE, FALSE))
  expect_identical(is_efo_id2(c("1234567", "EFO_1234567", NA_character_), convert_NA_to_FALSE = FALSE), c(TRUE, TRUE, NA))
  expect_identical(is_efo_id2(c('GO_0097334', 'HP_0001268', 'Orphanet_182098', 'NCIT_C74532')), c(TRUE, TRUE, TRUE, TRUE))
  expect_error(is_efo_id2(1L), "str argument must be a character vector.")
  expect_error(is_efo_id2(character()), "str contains no values, it must contain at least one string.")
})

#
## is_study_id
#
test_that("is_study_id", {
  expect_true(is_study_id("GCST123456"))
  expect_true(is_study_id("GCST90002423"))
  expect_identical(is_study_id(c("GCST123456", "GCST000042", "000042")), c(TRUE, TRUE, FALSE))
  expect_identical(is_study_id(c("GCST123456", "GCST", NA_character_)), c(TRUE, FALSE, FALSE))
  expect_identical(is_study_id(c("GCST123456", "GCST", NA_character_), convert_NA_to_FALSE = FALSE), c(TRUE, FALSE, NA))
  expect_error(is_study_id(1L), "str argument must be a character vector.")
  expect_error(is_study_id(character()), "str contains no values, it must contain at least one string.")
})

#
## is_pubmed_id
#
test_that("is_pubmed_id", {
  expect_true(is_pubmed_id("132412"))
  expect_identical(is_pubmed_id(c("1324", "1324231", "1")), c(TRUE, TRUE, TRUE))
  expect_identical(is_pubmed_id(''), FALSE)
  expect_identical(is_pubmed_id(c("1324", "1", NA_character_), convert_NA_to_FALSE = FALSE), c(TRUE, TRUE, NA))
  expect_error(is_pubmed_id(1L), "str argument must be a character vector.")
  expect_error(is_pubmed_id(character()), "str contains no values, it must contain at least one string.")
})

#
## contains_question_mark
#
test_that("contains_question_mark", {
  expect_true(contains_question_mark("?"))
  expect_identical(contains_question_mark(c("foo?bar", "42?yes!", "??")), c(TRUE, TRUE, TRUE))
  expect_identical(contains_question_mark(c('?nice', 'nice?', 'not nice')), c(TRUE, TRUE, FALSE))
  expect_identical(contains_question_mark(NA_character_, convert_NA_to_FALSE = TRUE), FALSE)
  expect_identical(contains_question_mark(NA_character_, convert_NA_to_FALSE = FALSE), NA)
  expect_error(contains_question_mark(1L), "str argument must be a character vector.")
  expect_error(contains_question_mark(character()), "str contains no values, it must contain at least one string.")
})

#
## is_empty_str
#
test_that("is_empty_str", {
  expect_true(is_empty_str(""))
  expect_true(is_empty_str(" "))
  expect_true(is_empty_str("      "))
  expect_identical(is_empty_str(c("", " ", "      ")), c(TRUE, TRUE, TRUE))
  expect_identical(is_empty_str(c('not empty', ' leading space here', 'trailing space here ')), c(FALSE, FALSE, FALSE))
  expect_identical(is_empty_str(NA_character_, convert_NA_to_FALSE = TRUE), FALSE)
  expect_identical(is_empty_str(NA_character_, convert_NA_to_FALSE = FALSE), NA)
  expect_error(is_empty_str(1L), "str argument must be a character vector.")
  expect_error(is_empty_str(character()), "str contains no values, it must contain at least one string.")
})

#
## cytogenetic_band_to_genomic_range
#
test_that("cytogenetic_band_to_genomic_range: exceptions", {
  expect_error(cytogenetic_band_to_genomic_range(bands = 42L), 'bands argument must be a character vector.')
  expect_error(cytogenetic_band_to_genomic_range(bands = character()), 'bands contains no values, it must contain at least one string.')
  expect_error(cytogenetic_band_to_genomic_range(bands = 'atm'), 'These are not valid cytogenetic bands: atm.\nCheck `cytogenetic_bands` dataframe for valid names.')

})

test_that("cytogenetic_band_to_genomic_range: normal usage", {
  g1 <-
    structure(
      list(
        chromosome = c("22", "1", "8"),
        start = c(15000001L, 1L, 97900001L),
        end = c(17400000L, 2300000L, 100500000L)
      ),
      row.names = c(NA, -3L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  expect_identical(cytogenetic_band_to_genomic_range(bands = c('22q11.1', '1p36.33', '8q22.2')), g1)
})
