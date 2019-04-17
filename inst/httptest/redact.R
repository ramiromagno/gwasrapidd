# function (response) {
#   require(magrittr, quietly=TRUE)
#   response %>%
#     httptest::gsub_response("https\\://www.ebi.ac.uk/gwas/rest/api/", "gc/")
# }


# httptest::set_redactor(
#   list(
#     function (response) { httptest::gsub_response(response, "https\\://www.ebi.ac.uk/gwas/rest/api/", "gc/") },
#     function (response) { httptest::gsub_response(response, "https:\\://www.ebi.ac.uk/efo/", "efo/") },
#     function (response) { httptest::gsub_response(response, "https:\\://www.ebi.ac.uk/ols/api/ontologies/efo", "ols/") }
#   )
# )

httptest::set_redactor(function (response) {
  response %>%
    httptest::gsub_response("^https\\://www.ebi.ac.uk/gwas/rest/api/studies/", "gc/s/") %>%
    httptest::gsub_response("^https\\://www.ebi.ac.uk/gwas/rest/api/associations/", "gc/a/") %>%
    httptest::gsub_response("^https\\://www.ebi.ac.uk/gwas/rest/api/singleNucleotidePolymorphisms/", "gc/v/") %>%
    httptest::gsub_response("^https\\://www.ebi.ac.uk/gwas/rest/api/efoTraits/", "gc/t/") %>%
    httptest::gsub_response("^https\\://www.ebi.ac.uk/gwas/rest/api/", "gc/") %>%
    httptest::gsub_response("^https\\://www.ebi.ac.uk/efo/", "efo/") %>%
    httptest::gsub_response("^https\\://www.ebi.ac.uk/ols/api/ontologies/efo/", "ols/efo/") %>%
    httptest::gsub_response("^https://httpbin.org/", "httpbin/")
})
