context("test-get_studies")

#
## get_studies, all studies
#

# with_mock_api({
#   test_that("get_studies: get all studies non-interactively", {
#     my_studies <- get_studies(interactive = FALSE)
#     expect_is(my_studies, 'studies')
#   })
# })
#
# with_mock_api({
#   test_that("get_studies: get all studies interactively (yes answer)", {
#     with_mock(readline = function(...) 'y', my_studies <- get_studies(interactive = TRUE))
#     expect_is(my_studies, 'studies')
#   })
# })
#
# with_mock_api({
#   test_that("get_studies: get all studies interactively (no answer)", {
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
  test_that("get_studies: GCST001085", {
    my_study <- get_studies(study_id = 'GCST001085')
    expect_is(my_study, 'studies')
    expect_true('GCST001085' %in% my_study@studies$study_id)
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
  test_that("get_studies: GCST001085, GCST000392", {
    study_ids <- c('GCST001085', 'GCST000392')
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
    my_studies <- get_studies(association_id = c('25389945', '24299710'))
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
