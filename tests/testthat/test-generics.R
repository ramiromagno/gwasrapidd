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
  test_that("n(): 2 studies: GCST002420, GCST000392", {
    study_ids <- c('GCST002420', 'GCST000392')
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
  test_that("n(): 1 association: '15608'", {
    my_association <- get_associations(association_id = '15608')
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
    trait_ids <- c('EFO_0005924', 'EFO_0004291')
    my_traits <- get_traits(efo_id = trait_ids)
    expect_is(my_traits, 'traits')
    expect_identical(n(my_traits), 2L)
  })
})

test_that("n() on studies, testing 'unique' parameter", {
  expect_identical(n(studies_ex01), 2L)
  expect_identical(n(studies_ex02), 2L)
  expect_identical(n(bind(studies_ex01, studies_ex02), unique = FALSE), 4L)
  expect_identical(n(bind(studies_ex01, studies_ex02), unique = TRUE), 3L)
})


test_that("n() on associations, testing 'unique' parameter", {
  expect_identical(n(associations_ex01), 4L)
  expect_identical(n(associations_ex02), 3L)
  expect_identical(n(bind(associations_ex01, associations_ex02), unique = FALSE), 7L)
  expect_identical(n(bind(associations_ex01, associations_ex02), unique = TRUE), 6L)
})

test_that("n() on variants, testing 'unique' parameter", {
  expect_identical(n(variants_ex01), 3L)
  expect_identical(n(variants_ex02), 4L)
  expect_identical(n(bind(variants_ex01, variants_ex02), unique = FALSE), 7L)
  expect_identical(n(bind(variants_ex01, variants_ex02), unique = TRUE), 5L)
})

test_that("n() on traits, testing the 'unique' parameter", {
  expect_identical(n(traits_ex01), 3L)
  expect_identical(n(traits_ex02), 4L)
  expect_identical(n(bind(traits_ex01, traits_ex02), unique = FALSE), 7L)
  expect_identical(n(bind(traits_ex01, traits_ex02), unique = TRUE), 6L)
})

#
## filter_by_id: Filter GWAS Catalog S4 objects by identifier.
#

test_that("filter_by_id() studies", {
  my_studies <- filter_by_id(studies_ex01, 'GCST001585')
  expect_is(my_studies, 'studies')
  expect_identical(my_studies@studies$study_id, 'GCST001585')
  expect_identical(n(my_studies), 1L)
})

test_that("filter_by_id() associations", {
  my_associations <- filter_by_id(associations_ex01, '19537565')
  expect_is(my_associations, 'associations')
  expect_identical(my_associations@associations$association_id, '19537565')
  expect_identical(n(my_associations), 1L)
})

test_that("filter_by_id() variants", {
  my_variants <- filter_by_id(variants_ex01, 'rs56261590')
  expect_is(my_variants, 'variants')
  expect_identical(my_variants@variants$variant_id, 'rs56261590')
  expect_identical(n(my_variants), 1L)
})

test_that("filter_by_id() traits", {
  my_traits <- filter_by_id(traits_ex01, 'EFO_0004884')
  expect_is(my_traits, 'traits')
  expect_identical(my_traits@traits$efo_id, 'EFO_0004884')
  expect_identical(n(my_traits), 1L)
})

#
## `[`(): Subset operator for GWAS Catalog S4 objects.
#

### studies
with_mock_api({
  test_that("subsetting studies with `[`", {
    study_ids <- c('GCST002420', 'GCST000392')
    my_studies <- get_studies(study_id = study_ids)
    expect_is(my_studies, 'studies')
    expect_identical(my_studies, my_studies[])
    expect_identical(my_studies['GCST002420']@studies$study_id, 'GCST002420')
    expect_identical(my_studies['GCST002420'], my_studies[1])
    expect_identical(my_studies['GCST000392']@studies$study_id, 'GCST000392')
    expect_identical(my_studies['GCST000392'], my_studies[2])
  })
})

### associations
with_mock_api({
  test_that("subsetting associations with `[`", {
    association_ids <- c('15608', '24300113')
    my_associations <- get_associations(association_id = association_ids)
    expect_is(my_associations, 'associations')
    expect_identical(my_associations, my_associations[])
    expect_identical(my_associations['15608']@associations$association_id, '15608')
    expect_identical(my_associations['15608'], my_associations[1])
    expect_identical(my_associations['24300113']@associations$association_id, '24300113')
    expect_identical(my_associations['24300113'], my_associations[2])
  })
})

### variants
with_mock_api({
  test_that("subsetting variants with `[`", {
    my_variants <- get_variants(variant_id = c('rs3798440', 'rs7329174'))
    expect_is(my_variants, 'variants')
    expect_identical(my_variants, my_variants[])
    expect_identical(my_variants['rs3798440']@variants$variant_id, 'rs3798440')
    expect_identical(my_variants['rs3798440'], my_variants[1])
    expect_identical(my_variants['rs7329174']@variants$variant_id, 'rs7329174')
    expect_identical(my_variants['rs7329174'], my_variants[2])
  })
})

### traits
with_mock_api({
  test_that("subsetting traits with `[`", {
    trait_ids <- c('EFO_0005924', 'EFO_0004291')
    my_traits <- get_traits(efo_id = trait_ids)
    expect_is(my_traits, 'traits')
    expect_identical(my_traits, my_traits[])
    expect_identical(my_traits['EFO_0005924']@traits$efo_id, 'EFO_0005924')
    expect_identical(my_traits['EFO_0005924'], my_traits[1])
    expect_identical(my_traits['EFO_0004291']@traits$efo_id, 'EFO_0004291')
    expect_identical(my_traits['EFO_0004291'], my_traits[2])
  })
})

#
## Set operations: GWAS Catalog S4 studies.
#
with_mock_api({
  test_that("set operations on S4 studies", {
    s1 <- get_studies(study_id = 'GCST002420')
    s2 <- get_studies(study_id = 'GCST000392')
    s3 <- get_studies(study_id = c('GCST002420', 'GCST000392'))
    expect_is(s1, 'studies')
    expect_is(s2, 'studies')
    expect_is(s3, 'studies')
    # union
    expect_identical(union(s1, s2), s3)
    expect_identical(union(s3, s3), s3)
   # intersection
    expect_identical(intersect(s1, s3), s1)
    expect_identical(intersect(s2, s3), s2)
    expect_identical(intersect(s3, s3), s3)
    # setdiff
    expect_identical(setdiff(s3, s1), s2)
    expect_identical(setdiff(s3, s2), s1)
    expect_identical(setdiff(s3, s3), studies())
    # setequal
    expect_true(setequal(s1, s1))
    expect_true(setequal(union(s1, s2), union(s2, s1))) # setequal is invariant to order
  })
})

#
## Set operations: GWAS Catalog S4 associations.
#
with_mock_api({
  test_that("set operations on S4 associations", {
    a1 <- get_associations(association_id = '15608')
    a2 <- get_associations(association_id = '24300113')
    a3 <- get_associations(association_id = c('15608', '24300113'))
    expect_is(a1, 'associations')
    expect_is(a2, 'associations')
    expect_is(a3, 'associations')
    # union
    expect_identical(union(a1, a2), a3)
    expect_identical(union(a3, a3), a3)
    # intersection
    expect_identical(intersect(a1, a3), a1)
    expect_identical(intersect(a2, a3), a2)
    expect_identical(intersect(a3, a3), a3)
    # setdiff
    expect_identical(setdiff(a3, a1), a2)
    expect_identical(setdiff(a3, a2), a1)
    expect_identical(setdiff(a3, a3), associations())
    # setequal
    expect_true(setequal(a1, a1))
    expect_true(setequal(union(a1, a2), union(a2, a1))) # setequal is invariant to order
  })
})

#
## Set operations: GWAS Catalog S4 variants.
#
with_mock_api({
  test_that("set operations on S4 variants", {
    v1 <- get_variants(variant_id = 'rs3798440')
    v2 <- get_variants(variant_id = 'rs7329174')
    v3 <- get_variants(variant_id = c('rs3798440', 'rs7329174'))
    expect_is(v1, 'variants')
    expect_is(v2, 'variants')
    expect_is(v3, 'variants')
    # union
    expect_identical(union(v1, v2), v3)
    expect_identical(union(v3, v3), v3)
    # intersection
    expect_identical(intersect(v1, v3), v1)
    expect_identical(intersect(v2, v3), v2)
    expect_identical(intersect(v3, v3), v3)
    # setdiff
    expect_identical(setdiff(v3, v1), v2)
    expect_identical(setdiff(v3, v2), v1)
    expect_identical(setdiff(v3, v3), variants())
    # setequal
    expect_true(setequal(v1, v1))
    expect_true(setequal(union(v1, v2), union(v2, v1))) # setequal is invariant to order
  })
})

#
## Set operations: GWAS Catalog S4 traits.
#
with_mock_api({
  test_that("set operations on S4 traits", {
    t1 <- get_traits(efo_id = 'EFO_0005924')
    t2 <- get_traits(efo_id = 'EFO_0004291')
    t3 <- get_traits(efo_id = c('EFO_0005924', 'EFO_0004291'))
    expect_is(t1, 'traits')
    expect_is(t2, 'traits')
    expect_is(t3, 'traits')
    # union
    expect_identical(union(t1, t2), t3)
    expect_identical(union(t3, t3), t3)
    # intersection
    expect_identical(intersect(t1, t3), t1)
    expect_identical(intersect(t2, t3), t2)
    expect_identical(intersect(t3, t3), t3)
    # setdiff
    expect_identical(setdiff(t3, t1), t2)
    expect_identical(setdiff(t3, t2), t1)
    expect_identical(setdiff(t3, t3), traits())
    # setequal
    expect_true(setequal(t1, t1))
    expect_true(setequal(union(t1, t2), union(t2, t1))) # setequal is invariant to order
  })
})

#
## bind() on S4 studies.
#
with_mock_api({
  test_that("bind S4 studies", {
    s1 <- get_studies(study_id = 'GCST002420')
    s2 <- get_studies(study_id = 'GCST000392')
    s3 <- get_studies(study_id = c('GCST002420', 'GCST000392'))
    expect_is(s1, 'studies')
    expect_is(s2, 'studies')
    expect_is(s3, 'studies')
    expect_true(identical(bind(s1, s2), s3))
    expect_false(identical(bind(s2, s1), s3))
  })
})

#
## bind() on S4 associations.
#
with_mock_api({
  test_that("bind S4 associations", {
    a1 <- get_associations(association_id = '15608')
    a2 <- get_associations(association_id = '24300113')
    a3 <- get_associations(association_id = c('15608', '24300113'))
    expect_is(a1, 'associations')
    expect_is(a2, 'associations')
    expect_is(a3, 'associations')
    expect_true(identical(bind(a1, a2), a3))
    expect_false(identical(bind(a2, a1), a3))
  })
})

#
## bind() on S4 variants.
#
with_mock_api({
  test_that("bind S4 variants", {
    v1 <- get_variants(variant_id = 'rs3798440')
    v2 <- get_variants(variant_id = 'rs7329174')
    v3 <- get_variants(variant_id = c('rs3798440', 'rs7329174'))
    expect_is(v1, 'variants')
    expect_is(v2, 'variants')
    expect_is(v3, 'variants')
    expect_true(identical(bind(v1, v2), v3))
    expect_false(identical(bind(v2, v1), v3))
  })
})

#
## bind() on S4 traits.
#
with_mock_api({
  test_that("bind S4 traits", {
    t1 <- get_traits(efo_id = 'EFO_0005924')
    t2 <- get_traits(efo_id = 'EFO_0004291')
    t3 <- get_traits(efo_id = c('EFO_0005924', 'EFO_0004291'))
    expect_is(t1, 'traits')
    expect_is(t2, 'traits')
    expect_is(t3, 'traits')
    expect_true(identical(bind(t1, t2), t3))
    expect_false(identical(bind(t2, t1), t3))
  })
})
