function (request) {
  require(magrittr, quietly=TRUE)
  request %>%
    httptest::gsub_request("https\\://www.ebi.ac.uk/gwas/rest/api/", "gc/")
}

# httptest::set_requester(
#   list(
#     function (request) { httptest::gsub_request(request, "https\\://www.ebi.ac.uk/gwas/rest/api/", "gc/") },
#     function (request) { httptest::gsub_request(request, "https:\\://www.ebi.ac.uk/efo/", "efo/") },
#     function (request) { httptest::gsub_request(request, "https:\\://www.ebi.ac.uk/ols/api/ontologies/efo", "ols/") }
#   )
# )
