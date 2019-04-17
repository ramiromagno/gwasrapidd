httptest::set_requester(function (response) {
  response %>%
    httptest::gsub_request("^https\\://www.ebi.ac.uk/gwas/rest/api/studies/", "mocks/gc/s/") %>%
    httptest::gsub_request("^https\\://www.ebi.ac.uk/gwas/rest/api/associations/", "mocks/gc/a/") %>%
    httptest::gsub_request("^https\\://www.ebi.ac.uk/gwas/rest/api/singleNucleotidePolymorphisms/", "mocks/gc/v/") %>%
    httptest::gsub_request("^https\\://www.ebi.ac.uk/gwas/rest/api/efoTraits/", "mocks/gc/t/") %>%
    httptest::gsub_request("^https\\://www.ebi.ac.uk/gwas/rest/api/", "mocks/gc/") %>%
    httptest::gsub_request("^https\\://www.ebi.ac.uk/efo/", "mocks/efo/") %>%
    httptest::gsub_request("^https\\://www.ebi.ac.uk/ols/api/ontologies/efo", "mocks/ols/efo/") %>%
    httptest::gsub_request("^https\\://httpbin.org/", "mocks/httpbin/")
})
