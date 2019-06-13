context('test-post-traits')

with_mock_api({
  test_that('child_efo_ids', {
    # EFO_0004761 has no children so should return an empty character vector.
    children1 <- child_efo_ids(efo_id = 'EFO_0005924')
    expect_identical(children1, character())
    children2 <- child_efo_ids(efo_id = 'EFO_0005106')
    expected_children <- c("EFO_0007800", "EFO_0004341")
    expect_setequal(children2, expected_children)
  })
})

with_mock_api({
  test_that('get_child_efo', {
    # EFO_0005924 has no children so should return an empty character vector.
    results <- get_child_efo(efo_id = c('EFO_0005924', 'EFO_0005106'))
    expect_named(results,  c('EFO_0005924', 'EFO_0005106'))
    EFO_0005924_children <- character()
    EFO_0005106_children <- c("EFO_0007800", "EFO_0004341")
    expect_identical(results, list(EFO_0005924 = EFO_0005924_children, EFO_0005106 = EFO_0005106_children))
  })
})
