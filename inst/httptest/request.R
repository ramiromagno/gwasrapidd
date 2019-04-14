function (request) {
  require(magrittr, quietly=TRUE)
  request %>%
    httptest::gsub_request("https\\://www.ebi.ac.uk/gwas/rest/api/", "gc/")
}
