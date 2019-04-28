context("test-parse-variants")

#
## filter_variants_by_standard_chromosomes
#

with_mock_api({
  test_that("filter_variants_by_standard_chromosomes: no non-standard locations", {
    my_variants <- get_variants(variant_id = c('rs3798440', 'rs7329174'))
    expect_is(my_variants, 'variants')
    my_variants2 <- filter_variants_by_standard_chromosomes(my_variants)
    expect_equal(my_variants, my_variants2)
  })
})

with_mock_api({
  test_that("filter_variants_by_standard_chromosomes: mitochondrial locations", {
    my_variants <- get_variants(variant_id = c('rs147903261', 'rs267606894'))
    expect_is(my_variants, 'variants')
    my_variants2 <- filter_variants_by_standard_chromosomes(my_variants)
    # By default filter_variants_by_standard_chromosomes should preserve variants
    # mapped to on the mitochondrial (MT) genome.
    expect_identical(my_variants, my_variants2)
  })
})

with_mock_api({
  test_that("filter_variants_by_standard_chromosomes: drop mitochondrial locations", {
    my_variants <- get_variants(variant_id = c('rs147903261', 'rs267606894'))
    expect_is(my_variants, 'variants')
    # Only allow autosomal and sex chromosomes (exclude the mitochondrial chr).
    my_variants2 <- filter_variants_by_standard_chromosomes(my_variants, chromosomes = c(1:22, 'X', 'Y'))
    expect_identical(my_variants2, variants())
  })
})

# Variants that also map to scaffolds other than the normal chromosomes
# rs10910092: maps to chr 1 and to CHR_HSCHR1_1_CTG3.
# rs570398477: maps to chr 2 and to CHR_HSCHR2_4_CTG1.
with_mock_api({
  test_that("filter_variants_by_standard_chromosomes: non-standard locations are dropped", {
    my_variants <- get_variants(variant_id = c('rs10910092', 'rs570398477'))
    expect_is(my_variants, 'variants')
    my_variants2 <- filter_variants_by_standard_chromosomes(my_variants)
    expect_identical(nrow(my_variants2@variants), 2L)
  })
})

test_that("filter_variants_by_standard_chromosomes: exceptions", {
  expect_error(filter_variants_by_standard_chromosomes(chromosomes = 'blah'), "These are not valid chromosome names: blah.")
})
