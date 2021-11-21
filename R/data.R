#' GRCh38 human cytogenetic bands.
#'
#' A dataset containing the GRCh38 human cytogenetic bands and their genomic
#' coordinates.
#'
#' Genomic coordinates are for
#' \href{https://genome-blog.soe.ucsc.edu/blog/wp-content/uploads/2016/12/newInterval.png}{fully
#' closed} intervals.
#'
#' @section Cytogenetic Nomenclature:
#' Cytogenetic bands are numbered from the centromere outwards in both
#' directions towards the telomeres on the shorter p arm and the longer q arm.
#'
#' The first number or letter represents the chromosome. Chromosomes 1 through
#' 22 (the autosomes) are designated by their chromosome number. The sex
#' chromosomes are designated by X or Y. The next letter represents the arm of
#' the chromosome: p or q.
#'
#' The numbers cannot be read in the normal decimal numeric system e.g. 36, but
#' rather 3-6 (region 3 band 6). Counting starts at the centromere as region 1
#' (or 1-0), to 11 (1-1) to 21 (2-1) to 22 (2-2) etc. Subbands are added in a
#' similar way, e.g. 21.1 to 21.2, if the bands are small or only appear at a
#' higher resolution.
#'
#'
#' @format A data frame with 862 rows and 8 variables:
#' \describe{
#'   \item{cytogenetic_band}{Cytogenetic band name. See \emph{Cytogenetic
#'   Nomenclature} below.}
#'   \item{chromosome}{Chromosome name: 1 through 22 (the autosomes), X or
#'   Y.}
#'   \item{start}{Genomic start position of the cytogenetic band. Starts at 1.}
#'   \item{end}{Genomic end position of the cytogenetic band. End position is
#'   included in the band interval.}
#'   \item{length}{Length of the genomic interval of cytogenetic band.}
#'   \item{assembly}{Assembly version, should be 'GRCh38'.}
#'   \item{stain}{\href{https://en.wikipedia.org/wiki/Giemsa_stain}{Giemsa
#'   stain} results: Giemsa negative, \code{'gneg'}; Giemsa positive, of
#'   increasing intensities, \code{'gpos25'}, \code{'gpos50'}, \code{'gpos75'},
#'   and \code{'gpos100'}; centromeric region, \code{'acen'}; heterochromatin,
#'   either pericentric or telomeric, \code{'gvar'}; and short arm of
#'   acrocentric chromosomes 13, 14, 15, 21, and 22 are coded as
#'   \code{'stalk'}.}
#'   \item{last_download_date}{Time stamp of last time this dataset was
#'   downloaded from Ensembl.}
#' }
#' @source \url{https://rest.ensembl.org/info/assembly/homo_sapiens?content-type=application/json&bands=1}
"cytogenetic_bands"


#' gwasrapidd entities' examples
#'
#' These are examples of GWAS Catalog entities shipped with gwasrapidd:
#'
#' @format
#' \describe{
#'   \item{studies_ex01}{An S4 \linkS4class{studies} object of 2 studies:
#'   \code{'GCST001585'} and \code{'GCST003985'}.}
#'   \item{studies_ex02}{An S4 \linkS4class{studies} object of 2 studies:
#'   \code{'GCST001585'} and \code{'GCST006655'}.}
#'   \item{associations_ex01}{An S4 \linkS4class{associations} object of 4
#'   associations: \code{'22509'}, \code{'22505'}, \code{'19537565'} and
#'   \code{'19537593'}.}
#'   \item{associations_ex02}{An S4 \linkS4class{associations} object of 3
#'   associations: \code{'19537593'}, \code{'31665940'} and \code{'34944736'}.}
#'   \item{variants_ex01}{An S4 \linkS4class{variants} object of 3 variants:
#'   \code{'rs146992477'}, \code{'rs56261590'} and \code{'rs4725504'}.}
#'   \item{variants_ex02}{An S4 \linkS4class{variants} object of 4 variants:
#'   \code{'rs56261590'}, \code{'rs4725504'}, \code{'rs11099757'} and
#'   \code{'rs16871509'}.}
#'   \item{traits_ex01}{An S4 \linkS4class{traits} object of 3 traits:
#'   \code{'EFO_0004884'}, \code{'EFO_0004343'} and \code{'EFO_0005299'}.}
#'   \item{traits_ex02}{An S4 \linkS4class{traits} object of 4 traits:
#'   \code{'EFO_0007845'}, \code{'EFO_0004699'}, \code{'EFO_0004884'} and
#'   \code{'EFO_0004875'}.}
#' }
#'
#' @name gc_examples

#' @rdname gc_examples
"studies_ex01"

#' @rdname gc_examples
"studies_ex02"

#' @rdname gc_examples
"associations_ex01"

#' @rdname gc_examples
"associations_ex02"

#' @rdname gc_examples
"variants_ex01"

#' @rdname gc_examples
"variants_ex02"

#' @rdname gc_examples
"traits_ex01"

#' @rdname gc_examples
"traits_ex02"
