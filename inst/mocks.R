library(httptest)

# Where to save the mocks
.mockPaths(NULL)
.mockPaths("tests/testthat/mocks")
options(httptest.verbose = TRUE)

#
# Mock API fixtures for requests in test-get_metadata.R
#
capture_requests({gwasrapidd::get_metadata()})

#
# Mock API fixtures for requests of studies
#
capture_requests({gwasrapidd::get_studies('GCST002420')})
capture_requests({gwasrapidd::get_studies('GCST000392')})
capture_requests({gwasrapidd::get_studies(association_id = '15608')})
capture_requests({gwasrapidd::get_studies(association_id = '24299710')})
capture_requests({gwasrapidd::get_studies(variant_id = 'rs3798440')})
capture_requests({gwasrapidd::get_studies(variant_id = 'rs7329174')})
capture_requests({gwasrapidd::get_studies(efo_id = 'EFO_0005924')})
capture_requests({gwasrapidd::get_studies(efo_id = 'EFO_0004291')})
capture_requests({gwasrapidd::get_studies(pubmed_id = '24882193')})
capture_requests({gwasrapidd::get_studies(pubmed_id = '22780124')})
capture_requests({gwasrapidd::get_studies(efo_uri = 'http://www.ebi.ac.uk/efo/EFO_0005924')})
capture_requests({gwasrapidd::get_studies(efo_uri = 'http://www.ebi.ac.uk/efo/EFO_0004291')})
capture_requests({gwasrapidd::get_studies(efo_trait = 'binge eating')})
capture_requests({gwasrapidd::get_studies(efo_trait = 'braces')})
capture_requests({gwasrapidd::get_studies(reported_trait = 'Common traits (Other)')})
capture_requests({gwasrapidd::get_studies(reported_trait = 'Binge eating behaviour in bipolar disorder')})

#
# Mock API fixtures for requests of associations
#
capture_requests({gwasrapidd::get_associations(association_id = '15608')})
capture_requests({gwasrapidd::get_associations(association_id = '24300113')})
capture_requests({gwasrapidd::get_associations(study_id = 'GCST002420')})
capture_requests({gwasrapidd::get_associations(variant_id = 'rs3798440')})
capture_requests({gwasrapidd::get_associations(variant_id = 'rs7329174')})
capture_requests({gwasrapidd::get_associations(pubmed_id = '24882193')})

#
# Mock API fixtures for requests of variants
#
capture_requests({gwasrapidd::get_variants(variant_id = 'rs3798440')})
capture_requests({gwasrapidd::get_variants(variant_id = 'rs7329174')})
capture_requests({gwasrapidd::get_variants(variant_id = 'rs10910092')})
capture_requests({gwasrapidd::get_variants(variant_id = 'rs570398477')})
capture_requests({gwasrapidd::get_variants(study_id = 'GCST002420')})
capture_requests({gwasrapidd::get_variants(association_id = '15608')})
capture_requests({gwasrapidd::get_variants(efo_id = 'EFO_0004291')})
capture_requests({gwasrapidd::get_variants(pubmed_id = '24882193')})
capture_requests({gwasrapidd::get_variants(
  genomic_range = list(chromosome = "22", start = 1L, end = "15473564"))
})
capture_requests({gwasrapidd::get_variants(cytogenetic_band = '3p21.32')})
capture_requests({gwasrapidd::get_variants(gene_name = 'AGKP1')})
capture_requests({gwasrapidd::get_variants(efo_trait = 'braces')})

# Mitochondrial genome variants
capture_requests({
  gwasrapidd::get_variants(variant_id = 'rs147903261')
  gwasrapidd::get_variants(variant_id = 'rs267606894')
})
# Variants that also map to scaffolds other than the normal chromosomes
# rs10910092: maps to chr 1 and to CHR_HSCHR1_1_CTG3.
# rs570398477: maps to chr 2 and to CHR_HSCHR2_4_CTG1.
capture_requests({
  gwasrapidd::get_variants(variant_id = 'rs10910092')
  gwasrapidd::get_variants(variant_id = 'rs570398477')
})

#
# Mock API fixtures for requests of traits
#
capture_requests({gwasrapidd::get_traits(efo_id = 'EFO_0005924')})
capture_requests({gwasrapidd::get_traits(efo_id = 'EFO_0004291')})
capture_requests({gwasrapidd::get_traits(study_id = 'GCST002420')})
capture_requests({gwasrapidd::get_traits(association_id = '15608')})
capture_requests({gwasrapidd::get_traits(pubmed_id = '22780124')})
capture_requests({gwasrapidd::get_traits(efo_uri = 'http://www.ebi.ac.uk/efo/EFO_0005924')})
capture_requests({gwasrapidd::get_traits(efo_trait = 'braces')})

#
# Mock API fixtures for requests in test-request.R
#

capture_requests({
  gwasrapidd:::gc_request(
    base_url = "https://httpbin.org",
    resource_url = "/html",
    warnings = FALSE)
})
capture_requests({gwasrapidd:::gc_request_all('/studies/GCST000854')})
capture_requests({gc_request_all('/studies/GCSTXXXXXX')})
capture_requests({
  gwasrapidd:::gc_request_all(
    resource_url = '/ontologies/efo/descendants?id=EFO_0009285',
    base_url = 'https://www.ebi.ac.uk/ols/api')
})
capture_requests({gwasrapidd:::gc_request_all('/associations/24299710/')})
capture_requests({gwasrapidd:::gc_get('/associations/24299710/snps')})
capture_requests({gwasrapidd:::gc_get('/associations/24299710/efoTraits')})
capture_requests({gwasrapidd:::gc_get('/efoTraits/search/findByPubmedId?pubmedId=24882193')})

#
# Mock API fixtures for requests in test-post-traits.R
#
capture_requests({get_child_efo(efo_id = c('EFO_0005924', 'EFO_0005106'))})
