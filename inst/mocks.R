library(httptest)

# Where to save the mocks
.mockPaths(NULL)
.mockPaths("tests/testthat/")
options(httptest.verbose=TRUE)

#
# Request captures for functions in get_metadata.R
#
start_capturing()
get_metadata()
stop_capturing()

#
# Request captures for functions in request.R
#

start_capturing()
gwasrapidd:::gc_request()
gwasrapidd:::gc_request(base_url = "https://httpbin.org", resource_url = "/html")
gwasrapidd:::gc_request_all('/studies')
gwasrapidd:::gc_request_all('/singleNucleotidePolymorphisms/search/findByGene?geneName=BRCA1')
gwasrapidd:::gc_request_all('/studies/search/findByPublicationIdPubmedId?pubmedId=21626137')
gwasrapidd:::gc_request_all('/studies/GCST000854')
gwasrapidd:::gc_request_all('/studies/GCSTXXXXXX')
gwasrapidd:::gc_request_all(resource_url = '/ontologies/efo/descendants?id=EFO_0009285', base_url = 'https://www.ebi.ac.uk/ols/api')
gwasrapidd:::gc_request_all('/efoTraits')
gwasrapidd:::gc_request_all('/associations/24299710/')
gwasrapidd:::gc_request_all('/singleNucleotidePolymorphisms/rs3798440/')
gwasrapidd:::gc_request_all('/efoTraits/EFO_0000537/')

gwasrapidd:::gc_get('/studies/GCST001085/')
gwasrapidd:::gc_get('/studies/GCST000392/')
gwasrapidd:::gc_get('/studies/GCST001085/associations')
gwasrapidd:::gc_get('/studies/GCST001085/snps')
gwasrapidd:::gc_get('/studies/GCST001085/efoTraits')
gwasrapidd:::gc_get('/associations/24299710/study')
gwasrapidd:::gc_get('/associations/25389945/study')
gwasrapidd:::gc_get('/associations/24299710/')
gwasrapidd:::gc_get('/associations/24299710/snps')
gwasrapidd:::gc_get('/associations/24299710/efoTraits')
gwasrapidd:::gc_get('/singleNucleotidePolymorphisms/rs3798440/studies')
gwasrapidd:::gc_get('/singleNucleotidePolymorphisms/rs3798440/associations')
gwasrapidd:::gc_get('/singleNucleotidePolymorphisms/rs3798440/')
gwasrapidd:::gc_get('/efoTraits/EFO_0000537/studies')
gwasrapidd:::gc_get('/efoTraits/EFO_0000537/associations')
gwasrapidd:::gc_get('/efoTraits/EFO_0000537/')
gwasrapidd:::gc_get('/studies/search/findByPublicationIdPubmedId?pubmedId=21626137')
gwasrapidd:::gc_get('/associations/search/findByPubmedId?pubmedId=21626137')
gwasrapidd:::gc_get('/singleNucleotidePolymorphisms/search/findByPubmedId?pubmedId=21626137')
gwasrapidd:::gc_get('/efoTraits/search/findByPubmedId?pubmedId=21626137')
gwasrapidd:::gc_get('/studies/search/findByUserRequested?userRequested=false')
gwasrapidd:::gc_get('/studies/search/findByUserRequested?userRequested=false')
gwasrapidd:::gc_get('/studies/search/findByEfoUri?uri=http://www.ebi.ac.uk/efo/EFO_0004761')
gwasrapidd:::gc_get('/efoTraits/search/findByEfoUri?uri=http://www.ebi.ac.uk/efo/EFO_0004761')
gwasrapidd:::gc_get('/associations/search/findByRsIdAndAccessionId?rsId=rs16956936&accessionId=GCST000392')
gwasrapidd:::gc_get('/singleNucleotidePolymorphisms/search/findByChromBpLocationRange?chrom=22&bpStart=1&bpEnd=15500000')
gwasrapidd:::gc_get('/singleNucleotidePolymorphisms/search/findByGene?geneName=BRCA1')
gwasrapidd:::gc_get('/studies/search/findByEfoTrait?efoTrait=lung%20adenocarcinoma')
gwasrapidd:::gc_get('/associations/search/findByEfoTrait?efoTrait=lung%20adenocarcinoma')
gwasrapidd:::gc_get('/associations/search/findByEfoTrait?efoTrait=lung%20adenocarcinoma')
gwasrapidd:::gc_get('/efoTraits/search/findByEfoTrait?trait=lung%20adenocarcinoma')
gwasrapidd:::gc_get('/studies/search/findByDiseaseTrait?diseaseTrait=Breast%20cancer')
gwasrapidd:::gc_get('/singleNucleotidePolymorphisms/search/findByDiseaseTrait?diseaseTrait=Breast%20cancer')
gwasrapidd::get_studies(interactive = FALSE)
stop_capturing()

capture_requests({
  gwasrapidd:::gc_get('/studies/search/findByEfoUri?uri=http://www.ebi.ac.uk/efo/EFO_0004761')
})
#
start_capturing()
gwasrapidd:::gc_get('/singleNucleotidePolymorphisms/rs7329174/')
gwasrapidd:::gc_get('/singleNucleotidePolymorphisms/rs7329174/studies')
stop_capturing()


# test-get_studies.R
capture_requests({
  gwasrapidd:::gc_get('/efoTraits/EFO_0000537/studies')
  gwasrapidd:::gc_get('/efoTraits/EFO_0000305/studies')
})

capture_requests({
  gwasrapidd:::gc_get('/studies/search/findByPublicationIdPubmedId?pubmedId=21626137')
  gwasrapidd:::gc_get('/studies/search/findByPublicationIdPubmedId?pubmedId=25890600')
})

capture_requests({
  gwasrapidd:::gc_get('/studies/search/findByUserRequested?userRequested=true')
  gwasrapidd:::gc_get('/studies/search/findByUserRequested?userRequested=false')
})

capture_requests({
  gwasrapidd:::gc_get('/studies/search/findByFullPvalueSet?fullPvalueSet=true')
  gwasrapidd:::gc_get('/studies/search/findByFullPvalueSet?fullPvalueSet=false')
})

# Note the translation of the slash '/' to %2f.
capture_requests({
  gwasrapidd:::gc_get('/studies/search/findByEfoUri?uri=http://www.ebi.ac.uk/efo%2fEFO_0004761')
  gwasrapidd:::gc_get('/studies/search/findByEfoUri?uri=http://www.ebi.ac.uk/efo%2fEFO_0000305')
})

capture_requests({
  gwasrapidd:::gc_get('/studies/search/findByEfoTrait?efoTrait=lung%20adenocarcinoma')
  gwasrapidd:::gc_get('/studies/search/findByEfoTrait?efoTrait=uric%20acid%20measurement')
})

capture_requests({
  gwasrapidd:::gc_get('/studies/search/findByDiseaseTrait?diseaseTrait=lung%20adenocarcinoma')
  gwasrapidd:::gc_get('/studies/search/findByDiseaseTrait?diseaseTrait=breast%20cancer')
})

# test-get_traits.R
capture_requests({
  gwasrapidd:::gc_get('/efoTraits/EFO_0000537/')
  gwasrapidd:::gc_get('/efoTraits/EFO_0000305/')
  gwasrapidd:::gc_get('/efoTraits/', page_size = 1000L)
})

capture_requests({
  gwasrapidd:::gc_get('/studies/GCST001085/efoTraits')
  gwasrapidd:::gc_get('/studies/GCST000392/efoTraits')
})

capture_requests({
  gwasrapidd:::gc_get('/associations/25389945/efoTraits')
  gwasrapidd:::gc_get('/associations/24299710/efoTraits')
})

capture_requests({
  gwasrapidd:::gc_get('/efoTraits/search/findByPubmedId?pubmedId=21626137', page_size = 1000L)
  gwasrapidd:::gc_get('/efoTraits/search/findByPubmedId?pubmedId=25890600', page_size = 1000L)
})

capture_requests({
  gwasrapidd:::gc_get('/efoTraits/search/findByEfoUri?uri=http://www.ebi.ac.uk/efo%2fEFO_0004761', page_size = 1000L)
  gwasrapidd:::gc_get('/efoTraits/search/findByEfoUri?uri=http://www.ebi.ac.uk/efo%2fEFO_0000305', page_size = 1000L)
})

capture_requests({
  gwasrapidd:::gc_get('/efoTraits/search/findByEfoTrait?trait=lung%20adenocarcinoma')
  gwasrapidd:::gc_get('/efoTraits/search/findByEfoTrait?trait=uric%20acid%20measurement')
})

# test-post-traits.R
capture_requests({
  gwasrapidd:::gc_request_all(base_url = 'https://www.ebi.ac.uk/ols/api/ontologies/efo', resource_url = '/descendants?id=EFO_0004761')
  gwasrapidd:::gc_request_all(base_url = 'https://www.ebi.ac.uk/ols/api/ontologies/efo', resource_url = '/descendants?id=EFO_0000305')
})

# test-get_variants.R
capture_requests({
  gwasrapidd:::gc_get('/studies/GCST001085/snps')
  gwasrapidd:::gc_get('/studies/GCST000392/snps')
})
capture_requests({
  gwasrapidd:::gc_get('/associations/25389945/snps')
  gwasrapidd:::gc_get('/associations/24299710/snps')
})

capture_requests({
  gwasrapidd:::gc_get('/singleNucleotidePolymorphisms/rs3798440/')
  gwasrapidd:::gc_get('/singleNucleotidePolymorphisms/rs7329174/')
})
capture_requests({
  gwasrapidd::get_variants(efo_id = c('EFO_0000537', 'EFO_0000305'))
})

capture_requests({
  gwasrapidd::get_variants(efo_id = 'EFO_0007990')
})
capture_requests({
  gwasrapidd:::gc_get('/singleNucleotidePolymorphisms/search/findByPubmedId?pubmedId=21626137')
})
capture_requests({
  gwasrapidd::get_studies(efo_trait = c("lung adenocarcinoma", "uric acid measurement"))
})
capture_requests({
  gwasrapidd:::gc_get('/singleNucleotidePolymorphisms/search/findByEfoTrait?efoTrait=lung%20adenocarcinoma')
  gwasrapidd:::gc_get('/singleNucleotidePolymorphisms/search/findByEfoTrait?efoTrait=uric%20acid%20measurement')
})
capture_requests({
  gwasrapidd::get_variants(genomic_range = list(chromosome = "22", start = 1L, end = "15473564"))
})
capture_requests({
  gwasrapidd::get_variants(gene_name = 'BRCA1')
})
