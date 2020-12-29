#
## study_to_association
#

test_that('study_to_association: normal usage', {
  out <- study_to_association(c('GCST001084', 'GCST001085'))
  expect_s3_class(out, 'data.frame')
  expect_length(out, n = 2L)
  expect_named(out, c('study_id', 'association_id'))
  expect_gt(nrow(out), 0L)
})

test_that('study_to_association: verbose', {
  expect_message(study_to_association(c('GCST001084'), verbose = TRUE), regexp = NULL)
})

test_that('study_to_association: warnings', {
  expect_warning(study_to_association(c('GCST0'), warnings = TRUE), regexp = NULL)
})

#
## study_to_variant
#

test_that('study_to_variant: normal usage', {
  out <- study_to_variant(c('GCST001084', 'GCST001085'))
  expect_s3_class(out, 'data.frame')
  expect_length(out, n = 2L)
  expect_named(out, c('study_id', 'variant_id'))
  expect_gt(nrow(out), 0L)
})

test_that('study_to_variant: verbose', {
  expect_message(study_to_variant(c('GCST001084'), verbose = TRUE), regexp = NULL)
})

test_that('study_to_variant: warnings', {
  expect_warning(study_to_variant(c('GCST0'), warnings = TRUE), regexp = NULL)
})

#
## study_to_trait
#

test_that('study_to_trait: normal usage', {
  out <- study_to_trait(c('GCST001084', 'GCST001085'))
  expect_s3_class(out, 'data.frame')
  expect_length(out, n = 2L)
  expect_named(out, c('study_id', 'efo_id'))
  expect_gt(nrow(out), 0L)
})

test_that('study_to_trait: verbose', {
  expect_message(study_to_trait(c('GCST001084'), verbose = TRUE), regexp = NULL)
})

test_that('study_to_trait: warnings', {
  expect_warning(study_to_trait(c('GCST0'), warnings = TRUE), regexp = NULL)
})

#
## association_to_study
#

test_that('association_to_study: normal usage', {
  out <- association_to_study(c('24300097', '24299759'))
  expect_s3_class(out, 'data.frame')
  expect_length(out, n = 2L)
  expect_named(out, c('association_id', 'study_id'))
  expect_gt(nrow(out), 0L)
})

test_that('association_to_study: verbose', {
  expect_message(association_to_study(c('24300097'), verbose = TRUE), regexp = NULL)
})

test_that('association_to_study: warnings', {
  expect_warning(association_to_study(c('0'), warnings = TRUE), regexp = NULL)
})

#
## association_to_variant
#

test_that('association_to_variant: normal usage', {
  out <- association_to_variant(c('24300097', '24299759'))
  expect_s3_class(out, 'data.frame')
  expect_length(out, n = 2L)
  expect_named(out, c('association_id', 'variant_id'))
  expect_gt(nrow(out), 0L)
})

test_that('association_to_variant: verbose', {
  expect_message(association_to_variant(c('24300097'), verbose = TRUE), regexp = NULL)
})

test_that('association_to_variant: warnings', {
  expect_warning(association_to_variant(c('0'), warnings = TRUE), regexp = NULL)
})

#
## association_to_trait
#

test_that('association_to_trait: normal usage', {
  out <- association_to_trait(c('24300097', '24299759'))
  expect_s3_class(out, 'data.frame')
  expect_length(out, n = 2L)
  expect_named(out, c('association_id', 'efo_id'))
  expect_gt(nrow(out), 0L)
})

test_that('association_to_trait: verbose', {
  expect_message(association_to_trait(c('24300097'), verbose = TRUE), regexp = NULL)
})

test_that('association_to_trait: warnings', {
  expect_warning(association_to_trait(c('0'), warnings = TRUE), regexp = NULL)
})

#
## variant_to_study
#

test_that('variant_to_study: normal usage', {
  out <- variant_to_study(c('rs7904579', 'rs138331350'))
  expect_s3_class(out, 'data.frame')
  expect_length(out, n = 2L)
  expect_named(out, c('variant_id', 'study_id'))
  expect_gt(nrow(out), 0L)
})

test_that('variant_to_study: verbose', {
  expect_message(variant_to_study(c('rs7904579'), verbose = TRUE), regexp = NULL)
})

test_that('variant_to_study: warnings', {
  expect_warning(variant_to_study(c('rs0'), warnings = TRUE), regexp = NULL)
})

#
## variant_to_association
#

test_that('variant_to_association: normal usage', {
  out <- variant_to_association(c('rs7904579', 'rs138331350'))
  expect_s3_class(out, 'data.frame')
  expect_length(out, n = 2L)
  expect_named(out, c('variant_id', 'association_id'))
  expect_gt(nrow(out), 0L)
})

test_that('variant_to_association: verbose', {
  expect_message(variant_to_association(c('rs7904579'), verbose = TRUE), regexp = NULL)
})

test_that('variant_to_association: warnings', {
  expect_warning(variant_to_association(c('rs0'), warnings = TRUE), regexp = NULL)
})

#
## variant_to_trait
#

test_that('variant_to_trait: normal usage', {
  out <- variant_to_trait(c('rs7904579', 'rs138331350'))
  expect_s3_class(out, 'data.frame')
  expect_length(out, n = 2L)
  expect_named(out, c('variant_id', 'efo_id'))
  expect_gt(nrow(out), 0L)
})

test_that('variant_to_trait: keep_association_id', {
  out <- variant_to_trait(c('rs7904579', 'rs138331350'), keep_association_id = TRUE)
  expect_s3_class(out, 'data.frame')
  expect_length(out, n = 3L)
  expect_named(out, c('variant_id', 'association_id', 'efo_id'))
  expect_gt(nrow(out), 0L)
})


test_that('variant_to_trait: verbose', {
  expect_message(variant_to_trait(c('rs7904579'), verbose = TRUE), regexp = NULL)
})

test_that('variant_to_trait: warnings', {
  expect_warning(variant_to_trait(c('rs0'), warnings = TRUE), regexp = NULL)
})

#
## trait_to_study
#

test_that('trait_to_study: normal usage', {
  out <- trait_to_study(c('EFO_0005108', 'EFO_0005109'))
  expect_s3_class(out, 'data.frame')
  expect_length(out, n = 2L)
  expect_named(out, c('efo_id', 'study_id'))
  expect_gt(nrow(out), 0L)
})

test_that('trait_to_study: verbose', {
  expect_message(trait_to_study(c('EFO_0005108'), verbose = TRUE), regexp = NULL)
})

test_that('trait_to_study: warnings', {
  expect_warning(trait_to_study(c('EFO_0000000'), warnings = TRUE), regexp = NULL)
})

#
## trait_to_association
#

test_that('trait_to_association: normal usage', {
  out <- trait_to_association(c('EFO_0005108', 'EFO_0005109'))
  expect_s3_class(out, 'data.frame')
  expect_length(out, n = 2L)
  expect_named(out, c('efo_id', 'association_id'))
  expect_gt(nrow(out), 0L)
})

test_that('trait_to_association: verbose', {
  expect_message(trait_to_association(c('EFO_0005108'), verbose = TRUE), regexp = NULL)
})

test_that('trait_to_association: warnings', {
  expect_warning(trait_to_association(c('EFO_0000000'), warnings = TRUE), regexp = NULL)
})

#
## trait_to_variant
#

test_that('trait_to_variant: normal usage', {
  out <- trait_to_variant(c('EFO_0005108', 'EFO_0005109'))
  expect_s3_class(out, 'data.frame')
  expect_length(out, n = 2L)
  expect_named(out, c('efo_id', 'variant_id'))
  expect_gt(nrow(out), 0L)
})

test_that('trait_to_variant: verbose', {
  expect_message(trait_to_variant(c('EFO_0005108'), verbose = TRUE), regexp = NULL)
})

test_that('trait_to_variant: warnings', {
  expect_warning(trait_to_variant(c('EFO_0000000'), warnings = TRUE), regexp = NULL)
})
