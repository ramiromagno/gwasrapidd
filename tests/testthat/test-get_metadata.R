context("test-metadata")

test_that("metadata_lst on empty input", {

  lst0 <- list(
    ensembl_release_number = NA_integer_,
    genome_build_version = NA_character_,
    dbsnp_version = NA_integer_,
    usage_start_date = lubridate::ymd_hms())

  lst1 <- metadata_lst(
    ensembl_release_number = NA_integer_,
    genome_build_version = NA_character_,
    dbsnp_version = NA_integer_,
    usage_start_date = lubridate::ymd_hms())

  expect_identical(lst1, lst0)
  expect_type(lst1$ensembl_release_number, "integer")
  expect_type(lst1$genome_build_version, "character")
  expect_type(lst1$dbsnp_version, "integer")
  expect_identical(lubridate::is.POSIXct(lst1$usage_start_date), TRUE)
})

test_that("metadata_lst on typical input", {

  lst0 <- list(
    ensembl_release_number = 95L,
    genome_build_version = "GRCh38.p12",
    dbsnp_version = 151L,
    usage_start_date = lubridate::ymd_hms("2019-01-12T14:00:01.921+0000"))

  lst1 <- metadata_lst(
    ensembl_release_number = 95L,
    genome_build_version = "GRCh38.p12",
    dbsnp_version = 151L,
    usage_start_date = lubridate::ymd_hms("2019-01-12T14:00:01.921+0000"))

  expect_identical(lst1, lst0)
  expect_type(lst1$ensembl_release_number, "integer")
  expect_type(lst1$genome_build_version, "character")
  expect_type(lst1$dbsnp_version, "integer")
  expect_identical(lubridate::is.POSIXct(lst1$usage_start_date), TRUE)
})

test_that("metadata_lst on NULL input", {

  lst0 <- list(
    ensembl_release_number = NA_integer_,
    genome_build_version = NA_character_,
    dbsnp_version = NA_integer_,
    usage_start_date = lubridate::ymd_hms())

  lst1 <- metadata_lst(
    ensembl_release_number = NULL,
    genome_build_version = NULL,
    dbsnp_version = NULL,
    usage_start_date = NULL)

  expect_identical(lst1, lst0)
  expect_type(lst1$ensembl_release_number, "integer")
  expect_type(lst1$genome_build_version, "character")
  expect_type(lst1$dbsnp_version, "integer")
  expect_identical(lubridate::is.POSIXct(lst1$usage_start_date), TRUE)
})

test_that("metadata_lst: ensembl_release_number ", {

  expect_error(
    metadata_lst(
      ensembl_release_number =  c(10L, 20L),
      genome_build_version = NULL,
      dbsnp_version = NULL,
      usage_start_date = NULL
    ),
    "ensembl_release_number must be a scalar integer."
  )

  expect_error(
    metadata_lst(
      ensembl_release_number =  10,
      genome_build_version = NULL,
      dbsnp_version = NULL,
      usage_start_date = NULL
    ),
    "ensembl_release_number must be a scalar integer."
  )

})

test_that("metadata_lst: genome_build_version ", {

  expect_error(
    metadata_lst(
      ensembl_release_number =  NULL,
      genome_build_version = c('', ''),
      dbsnp_version = NULL,
      usage_start_date = NULL
    ),
    "genome_build_version must be a scalar character."
  )

  expect_error(
    metadata_lst(
      ensembl_release_number =  NULL,
      genome_build_version = 1L,
      dbsnp_version = NULL,
      usage_start_date = NULL
    ),
    "genome_build_version must be a scalar character."
  )

})

test_that("metadata_lst: usage_start_date ", {

  expect_error(
    metadata_lst(
      ensembl_release_number =  NULL,
      genome_build_version = NULL,
      dbsnp_version = NULL,
      usage_start_date = c('', '')
    ),
    "usage_start_date must be a scalar POSIXct object."
  )

  expect_error(
    metadata_lst(
      ensembl_release_number =  NULL,
      genome_build_version = NULL,
      dbsnp_version = NULL,
      usage_start_date = 12L
    ),
    "usage_start_date must be a scalar POSIXct object."
  )

})

test_that("metadata_lst: dbsnp_version ", {

  expect_error(
    metadata_lst(
      ensembl_release_number =  NULL,
      genome_build_version = NULL,
      dbsnp_version = c(1L, 2L),
      usage_start_date = NULL
    ),
    "dbsnp_version must be a scalar integer."
  )

  expect_error(
    metadata_lst(
      ensembl_release_number =  NULL,
      genome_build_version = NULL,
      dbsnp_version = '',
      usage_start_date = NULL
    ),
    "dbsnp_version must be a scalar integer."
  )

})

with_mock_api({
  test_that("get_metadata", {
    lst0 <-
      list(
        ensembl_release_number = 96L,
        genome_build_version = "GRCh38.p12",
        dbsnp_version = 151L,
        usage_start_date = structure(
          1554750002.157,
          class = c("POSIXct",  "POSIXt"),
          tzone = "UTC"
        )
      )
    expect_identical(get_metadata(), lst0)
  })
})

test_that("get_metadata",
          {
            lst0 <- metadata_lst()
            # Mock an unsuccessful request using code 404 but any code different from 200
            # would do.
            with_mock(
              `httr::status_code` = function(...)
                404L,
              expect_identical(get_metadata(warnings = FALSE), lst0)
            )
})

