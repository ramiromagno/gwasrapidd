context("test-study")

#
## is_study_accession
#

test_that("Test is_study_accession convert_NA_to_FALSE option.", {
  expect_equal(is_study_accession(NA_character_), as.logical(NA))
  expect_equal(is_study_accession(c(NA_character_, NA_character_)), as.logical(c(NA, NA)))
  expect_equal(is_study_accession(NA_character_, convert_NA_to_FALSE = TRUE), FALSE)
  expect_equal(is_study_accession(c("GCST000000", NA_character_), convert_NA_to_FALSE = TRUE), c(TRUE, FALSE))
})

test_that("Test is_study_accession errors.", {
  expect_error(is_study_accession(1), "str argument must be a character vector.")
  expect_error(is_study_accession(character(0)), "str contains no values, it must contain at least one string.")
})

test_that("Test is_study_accession.", {
  expect_equal(is_study_accession("GCST123456"),   TRUE)
  expect_equal(is_study_accession("GCST000000"),   TRUE)
  expect_equal(is_study_accession("GCST999999"),   TRUE)
  expect_equal(is_study_accession("GCST000001"),   TRUE)
  expect_equal(is_study_accession(" GCST000000"),  FALSE)
  expect_equal(is_study_accession("GCST000000 "),  FALSE)
  expect_equal(is_study_accession("GCST 000000 "), FALSE)
  expect_equal(is_study_accession("000000"),       FALSE)
  expect_equal(is_study_accession("CST000001"),    FALSE)
  expect_equal(is_study_accession("GCS000001"),    FALSE)
  expect_equal(is_study_accession("GCST12345"),    FALSE)
})

#
## publication_to_tibble
#
test_that("Test publication_to_tibble", {

  pub_content <- list(
    pubmedId = "21041247",
    publicationDate = "2010-11-01",
    publication = "Am J Psychiatry",
    title = "Genome-wide association study of suicide attempts in mood disorder patients.",
    author = list(fullname = "Perlis RH", orcid = NA),
    `_links` = list(
      studies = list(
        href = "https://www.ebi.ac.uk/gwas/rest/api/studies/GCST000854{?projection}", templated = TRUE)
      )
  )

  pub_tibble <- tibble::tibble(
    pubmedId = "21041247",
    publicationDate = "2010-11-01",
    publication = "Am J Psychiatry",
    title = "Genome-wide association study of suicide attempts in mood disorder patients.",
    `_links` = list(
      studies = list(
        href = "https://www.ebi.ac.uk/gwas/rest/api/studies/GCST000854{?projection}", templated = TRUE)
    ),
    author_fullname = "Perlis RH",
    author_orcid = NA
  )

  expect_identical(publication_to_tibble(pub_content), pub_tibble)
})

with_mock_api({
  test_that("study_content_to_tibble", {
    response <- request("/studies/GCST000854")
    expect_is(response, "list")
    top_elements <- names(response)
    expect_identical(top_elements, c("response_code", "content"))

    response_tibble <- tibble::tibble(
      initialSampleSize = "3,117 European ancestry Bipolar disorder cases, 1,273 European ancestry Major depressive disorder cases",
      gxe = FALSE,
      gxg = FALSE,
      snpCount = 1922309L,
      qualifier = NA_character_,
      imputed = TRUE,
      pooled = FALSE,
      studyDesignComment = NA_character_,
      accessionId = "GCST000854",
      fullPvalueSet = FALSE,
      userRequested = FALSE,
      ancestries = list(
        tibble::tibble(
          type = c("initial", "replication", "replication"),
          numberOfIndividuals = c(4390L, 2698L, 1649L),
          ancestralGroups = list(
            tibble::tibble(ancestralGroup = "European"),
            tibble::tibble(ancestralGroup = "European"),
            tibble::tibble(ancestralGroup = "NR")
          ),
          countryOfOrigin = list(list(), list(), list()),
          countryOfRecruitment = list(
            tibble::tibble(
              majorArea = "Europe",
              region = "Northern Europe",
              countryName = "U.K."
            ),
            tibble::tibble(
              majorArea = "Europe",
              region = "Western Europe",
              countryName = "Germany"
            ),
            tibble::tibble()
          )
        )
      ),
      genotypingTechnologies = "Genome-wide genotyping array",
      replicationSampleSize = "2,698 European ancestry Bipolar disorder cases, 1,649 Major depressive disorder cases",
      publicationInfo = list(
        tibble::tibble(
          pubmedId = "21041247",
          publicationDate = "2010-11-01",
          publication = "Am J Psychiatry",
          title = "Genome-wide association study of suicide attempts in mood disorder patients.",
          author_fullname = "Perlis RH",
          author_orcid = NA_character_
        )
      ),
      platforms_manufacturer = "Affymetrix",
      diseaseTrait_trait = "Suicide risk"
    )

    expect_identical(study_content_to_tibble(response$content),
                     response_tibble)
  })
})
