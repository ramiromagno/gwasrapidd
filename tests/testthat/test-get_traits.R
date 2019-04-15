context("test-get_traits")

with_mock_api({
  test_that("get_traits: get all traits", {
    my_traits <- get_traits()
    expect_is(my_traits, 'traits')
  })
})

#
## get_traits, exceptions
#
test_that("get_traits: exceptions", {
  expect_error(get_traits(set_operation = 'invalid_option'),
               "set_operation must be either 'union' or 'intersection'")
  expect_error(get_traits(verbose = 1L),
               "verbose must be either TRUE or FALSE")
  expect_error(get_traits(verbose = NA),
               "verbose must be either TRUE or FALSE")
  expect_error(get_traits(warnings = NA),
               "warnings must be either TRUE or FALSE")
})


## get_traits, by trait id
#

with_mock_api({
  test_that("get_traits: GCST001085", {
    my_trait <- get_traits(study_id = 'GCST001085')
    expect_is(my_trait, 'traits')
  })
})

with_mock_api({
  test_that("get_traits: GCST000392", {
    my_trait <- get_traits(study_id = 'GCST000392')
    expect_is(my_trait, 'traits')
  })
})


with_mock_api({
  test_that("get_traits: GCST001085, GCST000392", {
    study_ids <- c('GCST001085', 'GCST000392')
    my_traits <- get_traits(study_id = study_ids)
    expect_is(my_traits, 'traits')
  })
})


#
## get_traits, by association id
#
with_mock_api({
  test_that("get_traits: by association id", {
    my_traits <- get_traits(association_id = c('25389945', '24299710'))
    expect_is(my_traits, 'traits')
  })
})

#
## get_traits, by efo id
#
with_mock_api({
  test_that("get_traits: by efo id", {
    my_traits <- get_traits(efo_id = c('EFO_0000537', 'EFO_0000305'))
    expect_is(my_traits, 'traits')
  })
})

#
## get_traits, by pubmed id
#
with_mock_api({
  test_that("get_traits: by pubmed id", {
    my_traits <- get_traits(pubmed_id = c('21626137', '25890600'))
    expect_is(my_traits, 'traits')
  })
})

#
## get_traits, by efo_uri
#
with_mock_api({
  test_that("get_traits: by efo_uri", {
    my_traits <- get_traits(efo_uri = c('http://www.ebi.ac.uk/efo/EFO_0004761', 'http://www.ebi.ac.uk/efo/EFO_0000305'))
    expect_is(my_traits, 'traits')
  })
})

#
## get_traits, by efo_trait
#
with_mock_api({
  test_that("get_traits: by efo_trait", {
    my_traits <- get_traits(efo_trait = c("lung adenocarcinoma", "uric acid measurement"))
    expect_is(my_traits, 'traits')
  })
})
