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
request()

stop_capturing()