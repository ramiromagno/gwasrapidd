context("test-request")

#
# gc_request()
#

test_that("gc_request: verbose flag", {
  response <- expect_message(gc_request(verbose = TRUE))
  expect_is(response, "list")
  expect_named(response, c('url', 'response_code', 'status', 'content'))
})

test_that("gc_request: warnings flag", {
  response <- expect_warning(gc_request(resource_url = "/foo", warnings = TRUE))
  expect_is(response, "list")
  expect_named(response, c('url', 'response_code', 'status', 'content'))
})

with_mock_api({
  test_that("gc_request: try a resource that does not return JSON", {
    # Use here httpbin.org for this test
    expect_warning(
      response <- gc_request(base_url = "https://httpbin.org", resource_url = "/html", warnings = TRUE),
      "Response to https://httpbin.org/html did not return JSON!")
    expect_identical(response$status, "Response content was not application/json.")
  })
})

#
# add_object_tier()
#

test_that("add_object_tier: exceptions", {
  expect_error(add_object_tier(NULL, ''), "obj must be a list.")
  expect_error(add_object_tier(list(), 1), "obj_type must be a string.")
  expect_error(add_object_tier(list(), ''), "obj_type cannot be an empty string")
})

test_that("add_object_tier: obj is empty", {
  expect_identical(add_object_tier(list(), 'studies'), list(content = list(studies = NULL)))
})

test_that("add_object_tier: obj already has obj_type", {
  lst <- list(content = list(studies = NULL))
  expect_identical(add_object_tier(lst, 'studies'), lst)
})

test_that("add_object_tier: obj already has not obj_type", {
  lst <- list(content = list(x = NULL))
  lst2 <- list(content = list(studies =  list(x = NULL)))
  expect_identical(add_object_tier(lst, 'studies'), lst2)
})

#
# object_type_from_url()
#

test_that("object_type_from_url: studies", {
  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/studies/'), 'studies')
  expect_identical(object_type_from_url('/studies/'), 'studies')
  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/studies'), 'studies')
  expect_identical(object_type_from_url('/studies'), 'studies')
  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/studies/some_study'), 'studies')
  expect_identical(object_type_from_url('/studies/some_study'), 'studies')
  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/associations/some_study/study/'), 'studies')
  expect_identical(object_type_from_url('/associations/some_study/study'), 'studies')
  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/singleNucleotidePolymorphisms/rs123/studies/'), 'studies')
  expect_identical(object_type_from_url('/singleNucleotidePolymorphisms/rs123/studies/'), 'studies')
  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/efoTraits/EFO_0000537/studies/'), 'studies')
  expect_identical(object_type_from_url('/efoTraits/EFO_0000537/studies/'), 'studies')
})

test_that("object_type_from_url: associations", {
  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/studies/GCST004349/associations/'), 'associations')
  expect_identical(object_type_from_url('/studies/GCST004349/associations/'), 'associations')
  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/studies/GCST004349/associations'), 'associations')
  expect_identical(object_type_from_url('/studies/GCST004349/associations'), 'associations')

  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/associations/'), 'associations')
  expect_identical(object_type_from_url('/associations/'), 'associations')
  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/associations'), 'associations')
  expect_identical(object_type_from_url('/associations'), 'associations')

  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/singleNucleotidePolymorphisms/rs123/associations/'), 'associations')
  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/singleNucleotidePolymorphisms/rs123/associations'), 'associations')
  expect_identical(object_type_from_url('/singleNucleotidePolymorphisms/rs123/associations/'), 'associations')
  expect_identical(object_type_from_url('/singleNucleotidePolymorphisms/rs123/associations'), 'associations')

  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/efoTraits/EFO_0005543/associations/'), 'associations')
  expect_identical(object_type_from_url('/efoTraits/EFO_0005543/associations/'), 'associations')
  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/efoTraits/EFO_0005543/associations'), 'associations')
  expect_identical(object_type_from_url('/efoTraits/EFO_0005543/associations'), 'associations')

  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/associations/search/'), 'associations')
  expect_identical(object_type_from_url('/associations/search/'), 'associations')
  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/associations/search'), 'associations')
  expect_identical(object_type_from_url('/associations/search'), 'associations')
})

test_that("object_type_from_url: variants", {
  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/studies/GCST004349/snps/'), 'variants')
  expect_identical(object_type_from_url('/studies/GCST004349/snps/'), 'variants')
  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/studies/GCST004349/snps'), 'variants')
  expect_identical(object_type_from_url('/studies/GCST004349/snps'), 'variants')

  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/associations/31078931/snps/'), 'variants')
  expect_identical(object_type_from_url('/associations/31078931/snps/'), 'variants')
  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/associations/31078931/snps'), 'variants')
  expect_identical(object_type_from_url('/associations/31078931/snps'), 'variants')

  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/singleNucleotidePolymorphisms/'), 'variants')
  expect_identical(object_type_from_url('/singleNucleotidePolymorphisms/'), 'variants')
  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/singleNucleotidePolymorphisms'), 'variants')
  expect_identical(object_type_from_url('/singleNucleotidePolymorphisms'), 'variants')

  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/singleNucleotidePolymorphisms/search/'), 'variants')
  expect_identical(object_type_from_url('/singleNucleotidePolymorphisms/search/'), 'variants')
  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/singleNucleotidePolymorphisms/search'), 'variants')
  expect_identical(object_type_from_url('/singleNucleotidePolymorphisms/search'), 'variants')
  })

test_that("object_type_from_url: traits", {
  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/studies/GCST004349/efoTraits/'), 'traits')
  expect_identical(object_type_from_url('/studies/GCST004349/efoTraits/'), 'traits')
  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/studies/GCST004349/efoTraits'), 'traits')
  expect_identical(object_type_from_url('/studies/GCST004349/efoTraits'), 'traits')

  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/associations/31078936/efoTraits/'), 'traits')
  expect_identical(object_type_from_url('/associations/31078936/efoTraits/'), 'traits')
  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/associations/31078936/efoTraits'), 'traits')
  expect_identical(object_type_from_url('/associations/31078936/efoTraits'), 'traits')

  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/efoTraits'), 'traits')
  expect_identical(object_type_from_url('/efoTraits'), 'traits')
  expect_identical(object_type_from_url('https://www.ebi.ac.uk/gwas/rest/api/efoTraits/'), 'traits')
  expect_identical(object_type_from_url('/efoTraits/'), 'traits')
})

test_that("object_type_from_url: url does not match any object", {
  expect_error(object_type_from_url('http://blah'), "No pattern matched the URL: http://blah.")
})

test_that("object_type_from_url: exception: unlikely case of multiple matching", {
  with_mock(
    # There are 18 patterns in object_type_from_url
    # This mock will simulate a multiple match of the URL to all object types:
    # studies, associations, variants and traits, that should lead to an error.
    `stringr::str_detect` = function(...) rep(TRUE, 18L),
    expect_error(object_type_from_url('/studies/'))
  )
})


#
# is_paginated()
#
test_that("is_paginated", {
  expect_identical(is_paginated(list()), FALSE)
  expect_identical(is_paginated(list(content = NULL)), FALSE)
  expect_identical(is_paginated(list(page = NULL)), TRUE)
})

#
# gc_request_all()
#

with_mock_api({
  test_that("gc_request_all: typical paginated case", {
    my_studies <- gc_request_all('/studies/search/findByPublicationIdPubmedId?pubmedId=24882193')
    expect_identical(names(my_studies), c('url', 'response_code', 'status', 'content'))
    expect_identical(my_studies$response_code, 200L)
    expect_identical(my_studies$status, "OK")
  })
})

with_mock_api({
  test_that("gc_request_all: progress bar == FALSE", {
    my_studies <- gc_request_all('/studies/search/findByPublicationIdPubmedId?pubmedId=24882193', progress_bar = FALSE)
    expect_identical(names(my_studies), c('url', 'response_code', 'status', 'content'))
    expect_identical(my_studies$response_code, 200L)
    expect_identical(my_studies$status, "OK")
  })
})

with_mock_api({
  test_that("gc_request_all: no pages case", {
    my_studies <- gc_request_all('/studies/GCST000854')
    expect_identical(names(my_studies), c('url', 'response_code', 'status', 'content'))
    expect_identical(my_studies$response_code, 200L)
    expect_identical(my_studies$status, "OK")
  })
})

with_mock_api({
  test_that("gc_request_all: 404 response", {
    my_studies <- expect_warning(gc_request_all('/studies/GCSTXXXXXX'))
    expect_identical(names(my_studies), c('url', 'response_code', 'status', 'content'))
    expect_identical(my_studies$response_code, 404L)
  })
})

with_mock_api({
  test_that("gc_request_all: page_size out of bounds", {
    expect_error(
      gc_request_all('/studies/search/findByPublicationIdPubmedId?pubmedId=24882193', page_size = 1001L),
      "page_size must be an integer scalar between 1 and 1000!")
  })
})

with_mock_api({
  test_that("gc_request_all: has page obj but number of elements is zero", {
      lst <- gc_request_all(
        resource_url = '/ontologies/efo/descendants?id=EFO_0005924',
        base_url = 'https://www.ebi.ac.uk/ols/api')
  expect_identical(names(lst), c('url', 'response_code', 'status', 'content'))
  })
})

# with_mock_api({
#   test_that("gc_request_all: url does not contains_question_mark", {
#     gc_request_all('/efoTraits')
#   })
# })

# This test simulates is_ok having one or more FALSEs
# is_ok <- purrr::map_lgl(objs, ~ identical(.x$status, "OK"))
# with_mock_api({
#   test_that("gc_request_all: one or more pages failed to return 200", {
#     with_mock(
#       `purrr::map_lgl` = function(...)
#         FALSE,
#       expect_error(
#         gc_request_all('/efoTraits'),
#         "Failed to get all pages of /efoTraits!"
#       )
#     )
#   })
# })

#
# is_embedded()
#
test_that("is_embedded", {
  expect_identical(is_embedded(list(content = list(`_embedded` = NULL))), TRUE)
  expect_identical(is_embedded(list(content = list())), FALSE)
})

#
# normalise_obj()
#
with_mock_api({
  test_that("normalise_obj: object contains `_embedded`", {
    url_study <- '/studies/search/findByPublicationIdPubmedId?pubmedId=24882193'
    obj_study <- gc_request_all(url_study) # This returns an `_embedded` object
    obj <- normalise_obj(obj_study, resource_url = url_study)
    expect_identical(obj, obj_study)
  })
})

with_mock_api({
  test_that("normalise_obj: studies", {
    url_study <- '/studies/GCST000854'
    obj_study <- gc_request_all(url_study)
    nobj_study <- normalise_obj(obj_study, resource_url = url_study)
    expect_identical(rlang::has_name(obj_study$content, 'studies'), FALSE)
    expect_identical(rlang::has_name(nobj_study$content, 'studies'), TRUE)

    expect_identical(list(obj_study$content$platforms), nobj_study$content$studies$platforms)
    expect_identical(list(obj_study$content$ancestries), nobj_study$content$studies$ancestries)
    expect_identical(list(obj_study$content$genotypingTechnologies), nobj_study$content$studies$genotypingTechnologies)
  })
})

with_mock_api({
  test_that("normalise_obj: associations", {
    url <- '/associations/24299710/'
    obj <- gc_request_all(url)
    nobj <- normalise_obj(obj, resource_url = url)
    expect_identical(rlang::has_name(obj$content, 'associations'), FALSE)
    expect_identical(rlang::has_name(nobj$content, 'associations'), TRUE)

    expect_identical(list(obj$content$loci), nobj$content$associations$loci)
  })
})

with_mock_api({
  test_that("normalise_obj: variants", {
    url <- '/singleNucleotidePolymorphisms/rs3798440/'
    obj <- gc_request_all(url)
    nobj <- normalise_obj(obj, resource_url = url)
    expect_identical(rlang::has_name(obj$content, 'singleNucleotidePolymorphisms'), FALSE)
    expect_identical(rlang::has_name(nobj$content, 'singleNucleotidePolymorphisms'), TRUE)

    expect_identical(list(obj$content$locations), nobj$content$singleNucleotidePolymorphisms$locations)
  })
})

with_mock_api({
  test_that("normalise_obj: traits", {
    url <- '/efoTraits/EFO_0000537/'
    obj <- gc_request_all(url)
    nobj <- normalise_obj(obj, resource_url = url)
    expect_identical(rlang::has_name(obj$content, 'efoTraits'), FALSE)
    expect_identical(rlang::has_name(nobj$content, 'efoTraits'), TRUE)
  })
})

# This tests the condition that resource_url does not map to one of: studies,
# associations, variants or traits.
test_that("normalise_obj: invalid resource_url", {
  expect_error(normalise_obj(list(), resource_url = 'foo'), "No pattern matched the URL: foo.")
})

#
# peel_off_embedded()
#
test_that("peel_off_embedded", {
  json_list0 <- list(content = NULL)
  json_list1 <- list(content = list(`_embedded` = list('a', 'b')))
  json_list2 <- list(content = list('a', 'b'))

  expect_identical(peel_off_embedded(json_list0), json_list0)
  expect_identical(peel_off_embedded(json_list1), json_list2)
})

#
# gc_get()
#
with_mock_api({
  test_that("gc_get", {
    gc_get('/studies/GCST002420/')
    gc_get('/studies/GCST002420/associations')
    gc_get('/studies/GCST002420/snps')
    gc_get('/studies/GCST002420/efoTraits')
    gc_get('/associations/24299710/study')
    gc_get('/associations/24299710/')
    gc_get('/associations/24299710/snps')
    gc_get('/associations/24299710/efoTraits')
    gc_get('/singleNucleotidePolymorphisms/rs3798440/studies')
    gc_get('/singleNucleotidePolymorphisms/rs3798440/associations')
    gc_get('/singleNucleotidePolymorphisms/rs3798440/')
    gc_get('/efoTraits/EFO_0000537/studies')
    gc_get('/efoTraits/EFO_0000537/')
    gc_get('/studies/search/findByPublicationIdPubmedId?pubmedId=24882193')
    gc_get('/associations/search/findByPubmedId?pubmedId=24882193')
    gc_get('/singleNucleotidePolymorphisms/search/findByPubmedId?pubmedId=24882193')
    gc_get('/efoTraits/search/findByPubmedId?pubmedId=24882193')
  })
})

# test_that("gc_get: studies by efo_uri", {
#   base_url <- '/studies/search/findByEfoUri?uri='
#   uri <- 'http://www.ebi.ac.uk/efo/EFO_0004761'
#   uri_encoded <- URLencode(uri, reserved = TRUE)
#   url <- paste0(base_url, uri_encoded)
#   response <- gc_get(url)
#   expect_is(response, "list")
#   expect_named(response, c('url', 'response_code', 'status', 'content'))
#   expect_identical(response$status, c('OK', 'OK'))
# })

# test_that("gc_get: studies by efo_uri", {
#   base_url <- '/efoTraits/search/findByEfoUri?uri='
#   uri <- 'http://www.ebi.ac.uk/efo/EFO_0004761'
#   uri_encoded <- URLencode(uri, reserved = TRUE)
#   url <- paste0(base_url, uri_encoded)
#   response <- gc_get(url)
#   expect_is(response, "list")
#   expect_named(response, c('url', 'response_code', 'status', 'content'))
#   expect_identical(response$status, 'OK')
# })

test_that("gc_get: studies", {
  expect_error(gc_get(
    resource_url = c('/studies/GCST002420/', '/studies/GCST002420/')
  ),
  'resource_url must be a single string.')

  expect_error(
    gc_get(
      resource_url = '/studies/GCST002420/',
      base_url = c(
        'https://www.ebi.ac.uk/gwas/rest/api/',
        'https://www.ebi.ac.uk/gwas/rest/api/'
      )
    ),
    'base_url must be a single string.'
  )

  expect_error(
    gc_get('/studies/GCST002420/', page_size = -1L),
    "page_size must be an integer scalar between 1 and 1000!"
  )
  expect_error(
    gc_get('/studies/GCST002420/', page_size = 1001L),
    "page_size must be an integer scalar between 1 and 1000!"
  )
})
