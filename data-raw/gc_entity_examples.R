library(gwasrapidd)

studies_ex01 <- get_studies(study_id = c('GCST001585', 'GCST003985'))
studies_ex02 <- get_studies(study_id = c('GCST001585', 'GCST006655'))

associations_ex01 <- get_associations(association_id = c('22509', '22505', '19537565', '19537593'))
associations_ex02 <- get_associations(association_id = c('19537593', '31665940', '34944736'))

variants_ex01 <- get_variants(variant_id = c('rs146992477', 'rs56261590', 'rs4725504'))
variants_ex02 <- get_variants(variant_id = c('rs56261590', 'rs4725504', 'rs11099757', 'rs16871509'))

traits_ex01 <- get_traits(efo_id = c('EFO_0004884', 'EFO_0004343', 'EFO_0005299'))
traits_ex02 <- get_traits(efo_id = c('EFO_0007845', 'EFO_0004699', 'EFO_0004884', 'EFO_0004875'))

usethis::use_data(studies_ex01, compress = "xz", overwrite = TRUE, version = 2)
usethis::use_data(studies_ex02, compress = "xz", overwrite = TRUE, version = 2)
usethis::use_data(associations_ex01, compress = "xz", overwrite = TRUE, version = 2)
usethis::use_data(associations_ex02, compress = "xz", overwrite = TRUE, version = 2)
usethis::use_data(variants_ex01, compress = "xz", overwrite = TRUE, version = 2)
usethis::use_data(variants_ex02, compress = "xz", overwrite = TRUE, version = 2)
usethis::use_data(traits_ex01, compress = "xz", overwrite = TRUE, version = 2)
usethis::use_data(traits_ex02, compress = "xz", overwrite = TRUE, version = 2)
