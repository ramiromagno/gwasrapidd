function (response) {
  require(magrittr, quietly=TRUE)
  response %>%
    httptest::gsub_response("https\\://www.ebi.ac.uk/gwas/rest/api/", "gc/")
}


# httptest::set_redactor(
#   list(
#     function (response) { httptest::gsub_response(response, "https\\://www.ebi.ac.uk/gwas/rest/api/", "gc/") },
#     function (response) { httptest::gsub_response(response, "https:\\://www.ebi.ac.uk/efo/", "efo/") },
#     function (response) { httptest::gsub_response(response, "https:\\://www.ebi.ac.uk/ols/api/ontologies/efo", "ols/") }
#   )
# )
