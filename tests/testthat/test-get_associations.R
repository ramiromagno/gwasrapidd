context("test-get_associations")

#
## get_associations, exceptions
#
test_that("get_associations: exceptions", {
  expect_error(get_associations(set_operation = 'invalid_option'),
               "set_operation must be either 'union' or 'intersection'")
  expect_error(get_associations(verbose = 1L),
               "verbose must be either TRUE or FALSE")
  expect_error(get_associations(verbose = NA),
               "verbose must be either TRUE or FALSE")
  expect_error(get_associations(warnings = NA),
               "warnings must be either TRUE or FALSE")
})

#
## get_associations, by study id
#

with_mock_api({
  test_that("get_associations: GCST002420", {
    my_associations <- get_associations(study_id = 'GCST002420')
    expect_is(my_associations, 'associations')
  })
})

#
## get_associations, by association id
#
with_mock_api({
  test_that("get_associations: by association id", {
    my_associations <- get_associations(association_id = '15608')
    expect_is(my_associations, 'associations')
  })
})

#
## get_associations, by variant id
#
with_mock_api({
  test_that("get_associations: by variant id", {
    my_associations <- get_associations(variant_id = c('rs3798440', 'rs7329174'))
    expect_is(my_associations, 'associations')
  })
})

#
## get_associations, by efo id
#
# with_mock_api({
#   test_that("get_associations: by efo id", {
#     skip_if_testing_is_fast()
#     my_associations <- get_associations(efo_id = 'EFO_0007990')
#     expect_is(my_associations, 'associations')
#   })
# })

#
## get_associations, by pubmed id
#
with_mock_api({
  test_that("get_associations: by pubmed id", {
    my_associations <- get_associations(pubmed_id = '21626137')
    expect_is(my_associations, 'associations')
  })
})

#
## get_associations, by efo_trait
#
# with_mock_api({
#   test_that("get_associations: by efo_trait", {
#     skip_if_testing_is_fast()
#     my_associations <- get_associations(efo_trait = c("lung adenocarcinoma"))
#     expect_is(my_associations, 'associations')
#   })
# })

#
## exceptions
#

test_that("get_associations_by_study_id: study_id is NULL", {
  expect_identical(get_associations_by_study_id(), associations())
})
test_that("get_associations_by_study_id: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response,
            expect_identical(get_associations_by_study_id(study_id = 'GCST002420'), associations()))
})

test_that("get_associations_by_association_id: association_id is NULL", {
  expect_identical(get_associations_by_association_id(), associations())
})
test_that("get_associations_by_association_id: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response,
            expect_identical(get_associations(association_id = c('15608', '24299710')), associations()))
})

test_that("get_associations_by_variant_id: variant_id is NULL", {
  expect_identical(get_associations_by_variant_id(), associations())
})
test_that("get_associations_by_variant_id: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response,
            expect_identical(get_associations(variant_id = c('rs3798440', 'rs7329174')), associations()))
})


test_that("get_associations_by_efo_id: efo_id is NULL", {
  expect_identical(get_associations_by_efo_id(), associations())
})
test_that("get_associations_by_efo_id: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response,
            expect_identical(get_associations(efo_id =  'EFO_0007990'), associations()))
})

test_that("get_associations_by_pubmed_id: pubmed is NULL", {
  expect_identical(get_associations_by_pubmed_id(), associations())
})
test_that("get_associations_by_pubmed_id: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response,
            expect_identical(get_associations(pubmed_id = c('21626137', '25890600')), associations()))
})

test_that("get_associations_by_efo_trait: efo_trait is NULL", {
  expect_identical(get_associations_by_efo_trait(), associations())
})
test_that("get_associations_by_efo_trait: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response,
            expect_identical(get_associations(efo_trait = c("lung adenocarcinoma", "uric acid measurement")), associations()))
})
