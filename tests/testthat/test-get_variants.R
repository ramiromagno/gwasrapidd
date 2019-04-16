context("test-get_variants")

#
## get_studies, exceptions
#
test_that("get_variants: exceptions", {
  expect_error(get_variants(set_operation = 'invalid_option'),
               "set_operation must be either 'union' or 'intersection'")
  expect_error(get_variants(verbose = 1L),
               "verbose must be either TRUE or FALSE")
  expect_error(get_variants(verbose = NA),
               "verbose must be either TRUE or FALSE")
  expect_error(get_variants(warnings = NA),
               "warnings must be either TRUE or FALSE")
})

#
## get_variants, by study id
#

with_mock_api({
  test_that("get_variants: GCST001085", {
    my_variants <- get_variants(study_id = 'GCST001085')
    expect_is(my_variants, 'variants')
  })
})


#
## get_variants, by association id
#
with_mock_api({
  test_that("get_variants: by association id", {
    my_variants <- get_variants(association_id = '25389945')
    expect_is(my_variants, 'variants')
  })
})


#
## get_variants, by variant id
#
with_mock_api({
  test_that("get_variants: by variant id", {
    my_variants <- get_variants(variant_id = c('rs3798440', 'rs7329174'))
    expect_is(my_variants, 'variants')
  })
})

#
## get_variants, by efo id
#
with_mock_api({
  test_that("get_variants: by efo id", {
    skip_if_testing_is_fast()
    my_variants <- get_variants(efo_id = 'EFO_0007990')
    expect_is(my_variants, 'variants')
  })
})

#
## get_variants, by pubmed id
#
with_mock_api({
  test_that("get_variants: by pubmed id", {
    my_variants <- get_variants(pubmed_id = '21626137')
    expect_is(my_variants, 'variants')
  })
})

#
## get_variants, by genomic range
#
with_mock_api({
  test_that("get_variants: by genomic range", {
    my_variants <- get_variants(genomic_range = list(chromosome = "22", start = 1L, end = "15473564"))
    expect_is(my_variants, 'variants')
  })
})

#
## get_variants, by gene name
#
with_mock_api({
  test_that("get_variants: by gene name", {
    my_variants <- get_variants(gene_name = 'BRCA1')
    expect_is(my_variants, 'variants')
  })
})

#
## get_variants, by efo_trait
#
with_mock_api({
  test_that("get_variants: by efo_trait", {
    skip_if_testing_is_fast()
    my_variants <- get_variants(efo_trait = c("lung adenocarcinoma"))
    expect_is(my_variants, 'variants')
  })
})

# ATTENTION: This test cannot be properly done as the REST API is
# not currently supporting case insensitive queries.
#
## get_variants, by reported_trait
#
# with_mock_api({
#   test_that("get_variants: by reported_trait", {
#     my_variants <- get_variants(reported_trait = "breast cancer")
#     expect_is(my_variants, 'variants')
#   })
# })

with_mock_api({
  test_that("get_variants: by genomic range", {
    my_variants <- get_variants(genomic_range = list(chromosome = "22", start = 1L, end = "15473564"))
    expect_is(my_variants, 'variants')
  })
})

test_that("get_variants: by genomic range, exceptions", {
  my_variants <- get_variants(genomic_range = list(chromosome = NULL, start = NULL, end = NULL))
  expect_is(my_variants, 'variants')
  expect_identical(my_variants, variants())
})

test_that("get_variants_by_study_id: study_id is NULL", {
  expect_identical(get_variants_by_study_id(), variants())
})
test_that("get_variants_by_study_id: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response,
            expect_identical(get_variants_by_study_id(study_id = 'GCST001085'), variants()))
})

test_that("get_variants_by_association_id: association_id is NULL", {
  expect_identical(get_variants_by_association_id(), variants())
})
test_that("get_variants_by_association_id: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response,
            expect_identical(get_variants(association_id = c('25389945', '24299710')), variants()))
})

test_that("get_variants_by_variant_id: variant_id is NULL", {
  expect_identical(get_variants_by_variant_id(), variants())
})
test_that("get_variants_by_variant_id: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response,
            expect_identical(get_variants(variant_id = c('rs3798440', 'rs7329174')), variants()))
})


test_that("get_variants_by_efo_id: efo_id is NULL", {
  expect_identical(get_variants_by_efo_id(), variants())
})


test_that("get_variants_by_pubmed_id: pubmed is NULL", {
  expect_identical(get_variants_by_pubmed_id(), variants())
})
test_that("get_variants_by_pubmed_id: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response,
            expect_identical(get_variants(pubmed_id = c('21626137', '25890600')), variants()))
})

test_that("get_variants_by_efo_trait: efo_trait is NULL", {
  expect_identical(get_variants_by_efo_trait(), variants())
})
test_that("get_variants_by_efo_trait: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response,
            expect_identical(get_variants(efo_trait = c("lung adenocarcinoma", "uric acid measurement")), variants()))
})

test_that("get_variants_by_reported_trait: reported_trait is NULL", {
  expect_identical(get_variants_by_reported_trait(), variants())
})
test_that("get_variants_by_reported_trait: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response,
            expect_identical(get_variants(reported_trait = c("breast cancer", 'lung adenocarcinoma')), variants()))
})
