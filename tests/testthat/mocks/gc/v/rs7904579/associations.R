structure(list(url = "gc/v/rs7904579/associations", status_code = 500L, 
    headers = structure(list(server = "Apache-Coyote/1.1", `content-type` = "application/json;charset=UTF-8", 
        `strict-transport-security` = "max-age=0", date = "Thu, 25 Nov 2021 23:31:01 GMT", 
        `transfer-encoding` = "chunked", connection = "close"), class = c("insensitive", 
    "list")), all_headers = list(list(status = 500L, version = "HTTP/1.1", 
        headers = structure(list(server = "Apache-Coyote/1.1", 
            `content-type` = "application/json;charset=UTF-8", 
            `strict-transport-security` = "max-age=0", date = "Thu, 25 Nov 2021 23:31:01 GMT", 
            `transfer-encoding` = "chunked", connection = "close"), class = c("insensitive", 
        "list")))), cookies = structure(list(domain = logical(0), 
        flag = logical(0), path = logical(0), secure = logical(0), 
        expiration = structure(numeric(0), class = c("POSIXct", 
        "POSIXt")), name = logical(0), value = logical(0)), row.names = integer(0), class = "data.frame"), 
    content = charToRaw("{\"timestamp\":1637883061970,\"status\":500,\"error\":\"Internal Server Error\",\"exception\":\"org.springframework.dao.DataAccessResourceFailureException\",\"message\":\"Unable to acquire JDBC Connection; nested exception is org.hibernate.exception.JDBCConnectionException: Unable to acquire JDBC Connection\",\"path\":\"/gwas/rest/api/singleNucleotidePolymorphisms/rs7904579/associations\"}"), 
    date = structure(1637883061, class = c("POSIXct", "POSIXt"
    ), tzone = "GMT"), times = c(redirect = 0, namelookup = 2.2e-05, 
    connect = 0.054347, pretransfer = 0.121377, starttransfer = 0.204527, 
    total = 0.20457)), class = "response")
