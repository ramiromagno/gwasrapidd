context('test-post-traits')

with_mock_api({
  test_that('child_efo_ids', {
    # EFO_0004761 is uric acid measurement
    # https://www.ebi.ac.uk/ols/ontologies/efo/terms?iri=http%3A%2F%2Fwww.ebi.ac.uk%2Fefo%2FEFO_0004761
    # EFO_0004761 has no children so should return an empty character vector.
    children1 <- child_efo_ids(efo_id = 'EFO_0004761')
    expect_identical(children1, character())
    children2 <- child_efo_ids(efo_id = 'EFO_0000305')
    expected_children <- c("EFO_0000432", "EFO_0000580", "EFO_0000553", "EFO_0000304",
                           "EFO_1000984", "EFO_1000326", "EFO_1000382", "EFO_0000306", "Orphanet_227535",
                           "EFO_0000186", "EFO_0000281", "EFO_0000552", "EFO_1000047", "EFO_1000221",
                           "EFO_1000210", "EFO_1000294", "EFO_1000307", "EFO_1000143", "EFO_0008509",
                           "EFO_1000402", "EFO_0009443", "EFO_0009649", "EFO_1000650", "EFO_1000649",
                           "EFO_1000913", "Orphanet_145", "EFO_1000040", "EFO_1000053",
                           "EFO_1001969", "EFO_1000071", "EFO_0006861", "EFO_0005537", "EFO_1002010")
    expect_setequal(children2, expected_children)
  })
})

with_mock_api({
  test_that('get_child_efo', {
    # EFO_0004761 is uric acid measurement
    # https://www.ebi.ac.uk/ols/ontologies/efo/terms?iri=http%3A%2F%2Fwww.ebi.ac.uk%2Fefo%2FEFO_0004761
    # EFO_0004761 has no children so should return an empty character vector.
    results <- get_child_efo(efo_id = c('EFO_0004761', 'EFO_0000305'))
    expect_named(results,  c('EFO_0004761', 'EFO_0000305'))
    EFO_0004761_children <- character()
    EFO_0000305_children <- c("EFO_0000432", "EFO_0000580", "EFO_0000553", "EFO_0000304",
                              "EFO_1000984", "EFO_1000326", "EFO_1000382", "EFO_0000306", "Orphanet_227535",
                              "EFO_0000186", "EFO_0000281", "EFO_0000552", "EFO_1000047", "EFO_1000221",
                              "EFO_1000210", "EFO_1000294", "EFO_1000307", "EFO_1000143", "EFO_0008509",
                              "EFO_1000402", "EFO_0009443", "EFO_0009649", "EFO_1000650", "EFO_1000649",
                              "EFO_1000913", "Orphanet_145", "EFO_1000040", "EFO_1000053",
                              "EFO_1001969", "EFO_1000071", "EFO_0006861", "EFO_0005537", "EFO_1002010")
    expect_identical(results, list(EFO_0004761 = EFO_0004761_children, EFO_0000305 = EFO_0000305_children))
  })
})
