context("test-metadata")

test_that("get_metadata", {

  # Mock an unsuccessful request using code 404 but any code different from 200
  # would do.
  with_mock(`httr::status_code` = function(...) 404L,
            expect_warning(metadata <- get_metadata(), "")
  )
  empty_metadata <- c(
    ensembl_release_number = NA_character_,
    genome_build_version = NA_character_,
    dbsnp_version = NA_character_,
    usage_start_date = NA_character_
  )
  expect_identical(metadata, empty_metadata)

  with_mock_api({
    actual_metadata <- get_metadata()
    expected_metadata <- c(
      ensembl_release_number = "94",
      genome_build_version = "GRCh38.p12",
      dbsnp_version = "151",
      usage_start_date = "2018-10-02T13:00:02.054+0000"
    )
    expect_identical(actual_metadata, expected_metadata)
  })
})
