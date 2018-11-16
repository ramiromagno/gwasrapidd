library(httptest)

# Where to save the mocks
.mockPaths(NULL)
.mockPaths("tests/testthat/")
options(httptest.verbose=TRUE)

#
# Request captures for functions in request.R
#
start_capturing()
request()
request("/fernandopessoa")
request(base_url = "https://httpbin.org", resource_url = "/html")
stop_capturing()

#
# Request captures for functions in snp.R
#
start_capturing()
request("/singleNucleotidePolymorphisms/rs10910092")
request("/snpLocation/X:1-20000")
request("/snpLocation/X:1-10")
request("/snpLocation/6:1-250000")
stop_capturing()

#
# Request captures for functions in study.R
#
start_capturing()
request("/studies/GCST000854")
request("/studies/GCST000000")
stop_capturing()
