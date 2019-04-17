library(httptest)

# Where to save the mocks
.mockPaths(NULL)
.mockPaths("tests/testthat/mocks")
options(httptest.verbose=TRUE)

#
# Mock API fixtures for requests in test-get_metadata.R
#
capture_requests({
  gwasrapidd::get_metadata()
}) %>% invisible()

#
# Mock API fixtures for requests in test-get_traits.R
#
capture_requests({
  gwasrapidd::get_traits()
  gwasrapidd::get_traits(study_id = c('GCST001085', 'GCST000392'))
  gwasrapidd::get_traits(association_id = c('25389945', '24299710'))
  gwasrapidd::get_traits(efo_id = c('EFO_0000537', 'EFO_0000305'))
  gwasrapidd::get_traits(pubmed_id = c('21626137', '25890600'))
  gwasrapidd::get_traits(efo_uri = c('http://www.ebi.ac.uk/efo/EFO_0004761', 'http://www.ebi.ac.uk/efo/EFO_0000305'))
  gwasrapidd::get_traits(efo_trait = c("lung adenocarcinoma", "uric acid measurement"))
}) %>% invisible()

#
# Mock API fixtures for requests in test-post-traits.R
#
capture_requests({
  get_child_efo(efo_id = c('EFO_0004761', 'EFO_0000305'))
}) %>% invisible()

#
# Mock API fixtures for requests in test-get_variants.R
#
capture_requests({
  get_variants(study_id = 'GCST001085')
  get_variants(association_id = '25389945')
  get_variants(variant_id = c('rs3798440', 'rs7329174'))
  get_variants(efo_id = 'EFO_0007990')
  get_variants(pubmed_id = '21626137')
  get_variants(genomic_range = list(chromosome = "22", start = 1L, end = "15473564"))
  get_variants(gene_name = 'BRCA1')
  get_variants(efo_trait = c("lung adenocarcinoma"))
}) %>% invisible()

#
# Mock API fixtures for requests in test-get_studies.R
#
capture_requests({
  get_studies(interactive = FALSE)
}) %>% invisible()

capture_requests({
  get_studies(user_requested = TRUE)
  get_studies(user_requested = FALSE)
}) %>% invisible()

capture_requests({
  get_studies(full_pvalue_set = TRUE)
  get_studies(full_pvalue_set = FALSE)
}) %>% invisible()

capture_requests({
  get_studies(study_id = c('GCST001085', 'GCST000392'))
  get_studies(association_id = c('25389945', '24299710'))
  get_studies(variant_id = c('rs3798440', 'rs7329174'))
  get_studies(efo_id = c('EFO_0000537', 'EFO_0000305'))
  get_studies(pubmed_id = c('21626137', '25890600'))
  get_studies(efo_uri = c('http://www.ebi.ac.uk/efo/EFO_0004761', 'http://www.ebi.ac.uk/efo/EFO_0000305'))
  get_studies(efo_trait = c("lung adenocarcinoma", "uric acid measurement"))
  get_studies(reported_trait = c("breast cancer", 'lung adenocarcinoma'))
}) %>% invisible()

#
# Mock API fixtures for requests in test-request.R
#
capture_requests({
  gwasrapidd:::gc_request(base_url = "https://httpbin.org", resource_url = "/html", warnings = FALSE)
  gwasrapidd:::gc_request_all('/studies/GCST000854')
  gwasrapidd:::gc_request_all(resource_url = '/ontologies/efo/descendants?id=EFO_0009285', base_url = 'https://www.ebi.ac.uk/ols/api')
  gwasrapidd:::gc_request_all('/studies/GCST000854')
  gwasrapidd:::gc_request_all('/associations/24299710/')
  gwasrapidd:::gc_get('/associations/24299710/snps')
  gwasrapidd:::gc_get('/studies/GCST001085/associations')
}) %>% invisible()

capture_requests({
  base_url <- '/efoTraits/search/findByEfoUri?uri='
  uri <- 'http://www.ebi.ac.uk/efo/EFO_0004761'
  uri_encoded <- URLencode(uri, reserved = TRUE)
  url <- paste0(base_url, uri_encoded)
  gwasrapidd:::gc_get(url)
}) %>% invisible()

capture_requests({
  base_url <- '/studies/search/findByEfoUri?uri='
  uri <- 'http://www.ebi.ac.uk/efo/EFO_0004761'
  uri_encoded <- URLencode(uri, reserved = TRUE)
  url <- paste0(base_url, uri_encoded)
  gwasrapidd:::gc_get(url)
}) %>% invisible()

capture_requests({
  gwasrapidd:::gc_get('/singleNucleotidePolymorphisms/rs3798440/associations')
  gwasrapidd:::gc_get('/singleNucleotidePolymorphisms/rs3798440/')
  gwasrapidd:::gc_get('/efoTraits/EFO_0000537/studies')
  gwasrapidd:::gc_get('/efoTraits/EFO_0000537/associations')
  gwasrapidd:::gc_get('/efoTraits/EFO_0000537/')
  gwasrapidd:::gc_get('/studies/search/findByPublicationIdPubmedId?pubmedId=21626137')
  gwasrapidd:::gc_get('/associations/search/findByPubmedId?pubmedId=21626137')
  gwasrapidd:::gc_get('/singleNucleotidePolymorphisms/search/findByPubmedId?pubmedId=21626137')
  gwasrapidd:::gc_get('/efoTraits/search/findByPubmedId?pubmedId=21626137')
  gwasrapidd:::gc_get('/associations/search/findByRsIdAndAccessionId?rsId=rs16956936&accessionId=GCST000392')
  gwasrapidd:::gc_get('/singleNucleotidePolymorphisms/search/findByChromBpLocationRange?chrom=22&bpStart=1&bpEnd=15500000')
  gwasrapidd:::gc_get('/studies/search/findByEfoTrait?efoTrait=lung%20adenocarcinoma')
  gwasrapidd:::gc_get('/efoTraits/search/findByEfoTrait?trait=lung%20adenocarcinoma')
}) %>% invisible()
