context("test-get_studies")

#
## get_studies, all studies
#

# with_mock_api({
#   test_that("get_studies: get all studies non-interactively", {
#     skip_if_testing_is_fast()
#     my_studies <- get_studies(interactive = FALSE)
#     expect_is(my_studies, 'studies')
#   })
# })
#
# with_mock_api({
#   test_that("get_studies: get all studies interactively (yes answer)", {
#     skip_if_testing_is_fast()
#     with_mock(readline = function(...) 'y', my_studies <- get_studies(interactive = TRUE))
#     expect_is(my_studies, 'studies')
#   })
# })
#
# with_mock_api({
#   test_that("get_studies: get all studies interactively (no answer)", {
#     skip_if_testing_is_fast()
#     with_mock(readline = function(...) 'n', my_studies <- get_studies(interactive = TRUE))
#     expect_identical(my_studies, studies()) # returns empty studies object
#   })
# })

#
## get_studies, exceptions
#
test_that("get_studies: exceptions", {
  expect_error(get_studies(set_operation = 'invalid_option'),
               "set_operation must be either 'union' or 'intersection'")
  expect_error(get_studies(verbose = 1L),
               "verbose must be either TRUE or FALSE")
  expect_error(get_studies(verbose = NA),
               "verbose must be either TRUE or FALSE")
  expect_error(get_studies(warnings = NA),
               "warnings must be either TRUE or FALSE")
})

#
## get_studies, by study id
#

with_mock_api({
  test_that("get_studies: GCST002420", {
    my_study <- get_studies(study_id = 'GCST002420')
    expect_is(my_study, 'studies')
    expect_true('GCST002420' %in% my_study@studies$study_id)
  })
})

with_mock_api({
  test_that("get_studies: GCST000392", {
    my_study <- get_studies(study_id = 'GCST000392')
    expect_is(my_study, 'studies')
    expect_true('GCST000392' %in% my_study@studies$study_id)
  })
})

with_mock_api({
  test_that("get_studies: GCST002420, GCST000392", {
    study_ids <- c('GCST002420', 'GCST000392')
    my_studies <- get_studies(study_id = study_ids)
    expect_is(my_studies, 'studies')
    expect_true(all(study_ids %in% my_studies@studies$study_id))
  })
})

#
## get_studies, by association id
#
with_mock_api({
  test_that("get_studies: by association id", {
    my_studies <- get_studies(association_id = c('15608', '24299710'))
    expect_is(my_studies, 'studies')
  })
})


#
## get_studies, by variant id
#
with_mock_api({
  test_that("get_studies: by variant id", {
    my_studies <- get_studies(variant_id = c('rs3798440', 'rs7329174'))
    expect_is(my_studies, 'studies')
  })
})

#
## get_studies, by efo id
#
with_mock_api({
  test_that("get_studies: by efo id", {
    my_studies <- get_studies(efo_id = c('EFO_0000537', 'EFO_0004291'))
    expect_is(my_studies, 'studies')
  })
})

#
## get_studies, by pubmed id
#
with_mock_api({
  test_that("get_studies: by pubmed id", {
    my_studies <- get_studies(pubmed_id = c('24882193', '22780124'))
    expect_is(my_studies, 'studies')
  })
})

#
## get_studies, by user_requested
#
# with_mock_api({
#   test_that("get_studies: by user_requested", {
#     skip_if_testing_is_fast()
#     my_studies1 <- get_studies(user_requested = TRUE)
#     expect_is(my_studies1, 'studies')
#     my_studies2 <- get_studies(user_requested = FALSE)
#     expect_is(my_studies2, 'studies')
#   })
# })

#
## get_studies, by full_pvalue_set
#
# with_mock_api({
#   test_that("get_studies: by full_pvalue_set", {
#     skip_if_testing_is_fast()
#     my_studies1 <- get_studies(full_pvalue_set = TRUE)
#     expect_is(my_studies1, 'studies')
#     my_studies2 <- get_studies(full_pvalue_set = FALSE)
#     expect_is(my_studies2, 'studies')
#   })
# })

#
## get_studies, by efo_uri
#

with_mock_api({
test_that("get_studies: by efo_uri", {
  my_studies <-
    get_studies(
      efo_uri = c(
        'http://www.ebi.ac.uk/efo/EFO_0005924',
        'http://www.ebi.ac.uk/efo/EFO_0004291'
      )
    )
  expect_is(my_studies, 'studies')
})
})

#
## get_studies, by efo_trait
#
with_mock_api({
  test_that("get_studies: by efo_trait", {
    my_studies <- get_studies(efo_trait = c("braces", "binge eating"))
    expect_is(my_studies, 'studies')
  })
})

#
## get_studies, by reported_trait
#
with_mock_api({
  test_that("get_studies: by reported_trait", {
    my_studies <- get_studies(reported_trait = c('Common traits (Other)', 'Binge eating behaviour in bipolar disorder'))
    expect_is(my_studies, 'studies')
    rep_traits <- tolower(my_studies@studies$reported_trait)
    expect_true(all(rep_traits %in% c('common traits (other)', 'binge eating behaviour in bipolar disorder')))
  })
})


test_that("get_studies_by_study_id: study_id is NULL", {
  expect_identical(get_studies_by_study_id(), studies())
})
test_that("get_studies_by_study_id: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response,
            expect_identical(get_studies_by_study_id(study_id = 'GCST002420'), studies()))
})

test_that("get_studies_by_association_id: association_id is NULL", {
  expect_identical(get_studies_by_association_id(), studies())
})
test_that("get_studies_by_association_id: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response,
            expect_identical(get_studies(association_id = c('15608', '24299710')), studies()))
})

test_that("get_studies_by_variant_id: variant_id is NULL", {
  expect_identical(get_studies_by_variant_id(), studies())
})
test_that("get_studies_by_variant_id: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response,
            expect_identical(get_studies(variant_id = c('rs3798440', 'rs7329174')), studies()))
})


test_that("get_studies_by_efo_id: efo_id is NULL", {
  expect_identical(get_studies_by_efo_id(), studies())
})
test_that("get_studies_by_efo_id: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response,
            expect_identical(get_studies(efo_id = c('EFO_0000537', 'EFO_0004291')), studies()))
})

test_that("get_studies_by_pubmed_id: pubmed is NULL", {
  expect_identical(get_studies_by_pubmed_id(), studies())
})
test_that("get_studies_by_pubmed_id: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response,
            expect_identical(get_studies(pubmed_id = c('24882193', '22780124')), studies()))
})

test_that("get_studies_by_user_requested: user_requested is NULL", {
  expect_identical(get_studies_by_user_requested(), studies())
})
test_that("get_studies_by_user_requested: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response,
            expect_identical(get_studies(user_requested = TRUE), studies()))
})

test_that("get_studies_by_full_pvalue_set: full_pvalue_set is NULL", {
  expect_identical(get_studies_by_full_pvalue_set(), studies())
})
test_that("get_studies_by_full_pvalue_set: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response,
            expect_identical(get_studies(full_pvalue_set = TRUE), studies()))
})

test_that("get_studies_by_efo_uri: efo_uri is NULL: status code is not 200", {
  expect_identical(get_studies_by_efo_uri(), studies())
})
test_that("get_studies_by_efo_uri: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  efo_uris <- c('http://www.ebi.ac.uk/efo/EFO_0005924', 'http://www.ebi.ac.uk/efo/EFO_0004291')
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response, expect_identical(get_studies(efo_uri = efo_uris), studies()))
})

test_that("get_studies_by_efo_trait: efo_trait is NULL", {
  expect_identical(get_studies_by_efo_trait(), studies())
})
test_that("get_studies_by_efo_trait: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response,
            expect_identical(get_studies(efo_trait = c("binge eating", "braces")), studies()))
})

test_that("get_studies_by_reported_trait: reported_trait is NULL", {
  expect_identical(get_studies_by_reported_trait(), studies())
})
test_that("get_studies_by_reported_trait: status code is not 200", {
  bad_response <- list(response_code = 404L, status = 'Not OK', url = NA, content = NA)
  with_mock(`gwasrapidd:::gc_get` = function(...) bad_response,
            expect_identical(get_studies(reported_trait = c("braces", 'binge eating')), studies()))
})
