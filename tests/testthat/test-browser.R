context("test-browser")

test_that("open_in_gwas_catalog", {
  not_one_of <- '`gwas_catalog_entity` must be one of "study", "variant", "trait", "gene", "region" or "publication"'

  local_mocked_bindings(
    `browse_url` = function(...) TRUE
  )

  expect_true(open_in_gwas_catalog('blah'))
  expect_error(open_in_gwas_catalog(42L), "`identifier` must be a character vector.")
  expect_error(open_in_gwas_catalog('GCST000016', gwas_catalog_entity = 42),
               "`gwas_catalog_entity` must be a character vector")
  expect_error(open_in_gwas_catalog(NA_character_), "The following positions of `identifier` are NAs: 1.")
  expect_error(open_in_gwas_catalog(c(NA_character_, 'GCST000016', NA_character_)),
               "The following positions of `identifier` are NAs: 1 and 3.")
  expect_error(open_in_gwas_catalog(NA_integer_), "`identifier` must be a character vector.")
  expect_error(open_in_gwas_catalog(NA_real_), "`identifier` must be a character vector.")
  expect_true(open_in_gwas_catalog('blah', gwas_catalog_entity = 'study'))
  expect_true(open_in_gwas_catalog('blah', gwas_catalog_entity = 'variant'))
  expect_true(open_in_gwas_catalog('blah', gwas_catalog_entity = 'trait'))
  expect_true(open_in_gwas_catalog('blah', gwas_catalog_entity = 'gene'))
  expect_true(open_in_gwas_catalog('blah', gwas_catalog_entity = 'region'))
  expect_true(open_in_gwas_catalog('blah', gwas_catalog_entity = 'publication'))

})

# test_that("open_in_gwas_catalog", {
#   not_one_of <- '`gwas_catalog_entity` must be one of "study", "variant", "trait", "gene", "region" or "publication"'
#   with_mocked_bindings(
#     `browseURL` = function(...) TRUE,
#     expect_error(open_in_gwas_catalog('GCST000016', gwas_catalog_entity = c('a', 'b')),
#                  not_one_of),
#     expect_error(open_in_gwas_catalog('GCST000016', gwas_catalog_entity = NA_character_),
#                  not_one_of),
#     expect_error(open_in_gwas_catalog('blah', gwas_catalog_entity = 'bad_entity'),
#                  not_one_of)
#   )
# })
