## R CMD check results

0 errors | 0 warnings | 0 note

- Hopefully there are no more errors or warnings because of EBI's REST API server endpoints being down.
- There are some warnings happening in some Linux distributions, namely Ubuntu Linux 20.04.1 LTS and Fedora Linux. These warnings are:

```
Found the following (possibly) invalid URLs:
  URL: https://www.ensembl.org
    From: inst/doc/faq.html
    Status: Error
    Message: libcurl error code 60:
      	SSL certificate problem: unable to get local issuer certificate
      	(Status without verification: OK)
  URL: https://www.ensembl.org/info/docs/api/versions.html
    From: man/get_metadata.Rd
          man/metadata_lst.Rd
    Status: Error
    Message: libcurl error code 60:
      	SSL certificate problem: unable to get local issuer certificate
      	(Status without verification: OK)
  URL: https://www.ensembl.org/info/genome/genebuild/index.html
    From: man/associations-class.Rd
          man/variants-class.Rd
    Status: Error
    Message: libcurl error code 60:
      	SSL certificate problem: unable to get local issuer certificate
      	(Status without verification: OK)
  URL: https://www.ensembl.org/info/genome/variation/prediction/predicted_data.html
    From: man/variants-class.Rd
    Status: Error
    Message: libcurl error code 60:
      	SSL certificate problem: unable to get local issuer certificate
      	(Status without verification: OK)
```

I cannot replicate this on my machine, an Archlinux. My bet is that it might be related to: https://github.com/Ensembl/ensembl-rest/issues/427.
