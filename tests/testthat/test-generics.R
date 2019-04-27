context("test-generics")


#
## n(): The tally function for GWAS Catalog S4 objects.
#

### n() applied on studies
test_that("n(): empty_studies", {
  empty_studies <- studies()
  expect_identical(n(empty_studies), 0L)
})

test_that("n(): studies that are NA", {
  studies_tbl1 <-
    studies_tbl(
      study_id = NA_character_,
      reported_trait = 'blah',
      initial_sample_size = 'blah',
      replication_sample_size = 'blah',
      gxe = TRUE,
      gxg = TRUE,
      snp_count = 42,
      qualifier = 'blah',
      imputed = FALSE,
      pooled = FALSE,
      study_design_comment = 'blah',
      full_pvalue_set = FALSE,
      user_requested = TRUE
    )
  NA_studies <- studies(studies = studies_tbl1)
  # n() returns 0 studies because a study_id == NA_character_
  # is dropped inside studies().
  expect_identical(n(NA_studies), 0L)
})

with_mock_api({
  test_that("n(): 2 studies: GCST001085, GCST000392", {
    study_ids <- c('GCST001085', 'GCST000392')
    my_studies <- get_studies(study_id = study_ids)
    expect_is(my_studies, 'studies')
    expect_identical(n(my_studies), 2L)
  })
})

### n() applied on associations
test_that("n(): empty_associations", {
  empty_associations <- associations()
  expect_identical(n(empty_associations), 0L)
})

test_that("n(): associations that are NA", {
  associations_tbl1 <-
    associations_tbl(
      association_id = NA_character_,
      pvalue = 3.14,
      pvalue_description = 'blah',
      pvalue_mantissa = 42L,
      pvalue_exponent = 42L,
      multiple_snp_haplotype = TRUE,
      snp_interaction = TRUE,
      snp_type = 'blah',
      standard_error = 3.14,
      range = 'blah',
      or_per_copy_number = 3.13,
      beta_number = 3.14,
      beta_unit = 'blah',
      beta_direction = 'blah',
      beta_description = 'blah',
      last_mapping_date = lubridate::ymd_hms("2013-01-24 19:39:07.880-0600"),
      last_update_date = lubridate::ymd_hms("2013-01-24 19:39:07.880-0600")
    )
  NA_associations <- associations(associations = associations_tbl1)
  # n() returns 0 associations because an association_id == NA_character_
  # is dropped inside associations().
  expect_identical(n(NA_associations), 0L)
})

with_mock_api({
  test_that("n(): 1 associations: '25389945'", {
    my_association <- get_associations(association_id = '25389945')
    expect_is(my_association, 'associations')
    expect_identical(n(my_association), 1L)
  })
})

### n() applied on variants
test_that("n(): empty_variants", {
  empty_variants <- variants()
  expect_identical(n(empty_variants), 0L)
})

test_that("n(): variants that are NA", {
  variants_tbl1 <-
    variants_tbl(
      variant_id = NA_character_,
      merged = 42,
      functional_class = 'blah',
      chromosome_name = 'blah',
      chromosome_position = 42,
      chromosome_region = 'blah',
      last_update_date = lubridate::ymd_hms("2013-01-24 19:39:07.880-0600")
    )
  NA_variants <- variants(variants = variants_tbl1)
  # n() returns 0 variants because a variant_id == NA_character_
  # is dropped inside variants().
  expect_identical(n(NA_variants), 0L)
})

with_mock_api({
  test_that("n(): 2 variants: ", {
    variant_ids <- c('rs3798440', 'rs7329174')
    my_variants <- get_variants(variant_id = variant_ids)
    expect_is(my_variants, 'variants')
    expect_identical(n(my_variants), 2L)
  })
})


### n() applied on traits
test_that("n(): empty_traits", {
  empty_traits <- traits()
  expect_identical(n(empty_traits), 0L)
})

test_that("n(): traits that are NA", {
  traits_tbl1 <-
    traits_tbl(
      efo_id = NA_character_,
      trait = 'blah',
      uri = 'blah'
    )
  NA_traits <- traits(traits = traits_tbl1)
  # n() returns 0 traits because a study_id == NA_character_
  # is dropped inside traits().
  expect_identical(n(NA_traits), 0L)
})

with_mock_api({
  test_that("n(): 2 traits: ", {
    trait_ids <- c('EFO_0000537', 'EFO_0000305')
    my_traits <- get_traits(efo_id = trait_ids)
    expect_is(my_traits, 'traits')
    expect_identical(n(my_traits), 2L)
  })
})

#
## `[`(): Subset operator for GWAS Catalog S4 objects.
#
