#' GRCh38 human cytogenetic bands.
#'
#' A dataset containing the GRCh38 human cytogenetic bands and their genomic
#' coordinates.
#'
#' Genomic coordinates are for
#' \href{http://genome.ucsc.edu/blog/wp-content/uploads/2016/12/newInterval.png}{fully
#' closed} intervals.
#'
#' @section Cytogenetic Nomenclature:
#' Cytogenetic bands are numbered from the centromere outwards in both directions
#' towards the telomeres on the shorter p arm and the longer q arm.
#'
#' The first number or letter represents the chromosome. Chromosomes 1 through
#' 22 (the autosomes) are designated by their chromosome number. The sex
#' chromosomes are designated by X or Y. The next letter represents the arm of
#' the chromosome: p or q.
#'
#' The numbers cannot be read in the normal decimal numeric system e.g. 36, but
#' rather 3-6 (region 3 band 6). Counting starts at the centromer as region 1
#' (or 1-0), to 11 (1-1) to 21 (2-1) to 22 (2-2) etc. Subbands are added in a
#' similar way, eg. 21.1 to 21.2, if the bands are small or only appear at a
#' higher resolution.
#'

#'
#' @format A data frame with 862 rows and 7 variables:
#' \describe{
#'   \item{cytogenetic_band}{Cytogenetic band name. See \emph{Cytogenetic
#'   Nomenclature} below.}
#'   \item{chromosome}{Chromosome name: 1 through 22 (the autosomes), X or
#'   Y.}
#'   \item{start}{Genomic start position of the cytogenetic band. Starts at 1.}
#'   \item{end}{Genomic end position of the cytogenetic band. End position is
#'   included in the band interval.}
#'   \item{assembly}{Assembly version, should be 'GRCh38'.}
#'   \item{stain}{\href{https://en.wikipedia.org/wiki/Giemsa_stain}{Giemsa
#'   stain} results: Giemsa negative, \code{'gneg'}; Giemsa positive, of
#'   increasing intensities, \code{'gpos25'}, \code{'gpos50'}, \code{'gpos75'},
#'   and \code{'gpos100'}; centromeric region, \code{'acen'}; heterochomatin,
#'   either pericentric or telomeric, \code{'gvar'}; and short arm of
#'   acrocentric chromosomes 13, 14, 15, 21, and 22 are coded as
#'   \code{'stalk'}.}
#'   \item{last_download_date}{Time stamp of last time this dataset was
#'   downloaded from Ensembl.}
#' }
#' @source \url{https://rest.ensembl.org/info/assembly/homo_sapiens?content-type=application/json&bands=1}
"cytogenetic_bands"
