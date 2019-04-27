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

# with_mock_api({
#   test_that("filter_variants_by_standard_chromosomes: ", {
#     my_variants <- get_variants(variant_id = c('rs3798440', 'rs7329174'))
#     expect_is(my_variants, 'variants')
#     my_variants2 <- filter_variants_by_standard_chromosomes(my_variants)
#     expect_equal(my_variants, my_variants2)
#   })
# })
