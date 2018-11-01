chromosomes <- c(as.character(seq_len(22)), "X", "Y")


#' Is a string a valid human genomic range?
#'
#' Find which strings are valid genomic ranges, i.e., of the form
#' \{chr\}:\{start\}-\{end\} by checking: \itemize{ \item if \code{chr} is one
#' of the human chromosomes (1--22, X or Y); \item if \code{start} and
#' \code{end} are integer numbers (of at most 9 digits).}
#'
#' Please note that in the case of the sex chromosomes, X and Y must be
#' uppercase, and that no check is performed to ensure that \code{start}
#' is less or equal than \code{end}.
#'
#' @param str A character vector of putative genomic ranges of the form
#'   \{chr\}:\{start\}-\{end\}.
#' @param convert_NA_to_FALSE Whether to preserve \code{NA}
#'   (\code{convert_NA_to_FALSE = FALSE}) or whether to return \code{FALSE} when
#'   an \code{NA} is found (\code{convert_NA_to_FALSE = TRUE}).
#' @return Returns a logical vector of the same length as \code{str}, \code{TRUE} for strings that are valid
#' genomic ranges, and \code{FALSE} otherwise.
#' @examples
#' # Check a valid human genomic range
#' is_genomic_range(c("10:10-20")) # TRUE
#'
#' # Test a bunch of ranges:
#' is_genomic_range(
#'   c("10:10-20", "X:1-2000", "Y:34-123")) # TRUE TRUE TRUE
#'
#' # By default NAs are returned as they are.
#' is_genomic_range(
#'   c("10:10-20", "X:1-2000", NA_character_)) # TRUE TRUE NA
#'
#' # Use the argument convert_NA_to_FALSE = TRUE to get FALSE instead of NA.
#' is_genomic_range(
#'   c("10:10-20", "X:1-2000", NA_character_),
#'   convert_NA_to_FALSE = TRUE) # TRUE TRUE FALSE
#'
#' # Start and End positions must be at most a 9-digit number.
#' is_genomic_range(c("1:1-123456789", "1:1-0123456789")) # TRUE FALSE
#'
#' # The name of the chromosome should not include 'chr'
#' # to be a valid genome range.
#' is_genomic_range("chr1:1-10") # FALSE
#'
#' # No check is performed on the actual values of start and end
#' # to ensure that start <= end
#' is_genomic_range("1:10-5") # TRUE
#'
#' @export
is_genomic_range <- function(str, convert_NA_to_FALSE = FALSE) {

  if(!is.character(str))
    stop("str argument must be a character vector.")

  if(identical(length(str), 0L))
    stop("str contains no values, it must contain at least one string.")

  # Replace NA with "".
  if(convert_NA_to_FALSE) {
    str2 <- str
    str2[is.na(str)] <- ""
  } else {
    str2 <- str
  }

  # Possible human chromosomes regex
  chrom_regex <- stringr::str_c(chromosomes, collapse = "|")
  # Regex for genomic range: {chr}:{start}-{end}
  regex <- sprintf("^(%s):(\\d{1,9})-(\\d{1,9})$", chrom_regex)

  is_gen_range <- stringr::str_detect(str2, regex)

  return(is_gen_range)
}

#' Convert to genomic range strings.
#'
#' This function converts three vectors: \code{chr}, \code{start}, and \code{end}
#' to strings of the form \{chr\}:\{start\}-\{end\}.
#'
#' @param chr A character vector of human chromosome names (1--22, X or Y), case sensitive.
#' @param start An integer vector of start positions.
#' @param end An integer vector of end positions.
#' @param starting_position_index Use this argument to indicate if the positions
#' are 0-based (\code{0L}) or 1-based (\code{1L}). This value is used to check
#' if positions are equal or above this number.
#' @return Returns a character vector whose strings are genomic ranges of the
#' form \{chr\}:\{start\}-\{end\}.
#'
#' @examples
#'
#' as_genomic_range("1", 10000L, 20000L) # Returns "1:10000-20000"
#'
#' @export
as_genomic_range <- function(chr, start, end, starting_position_index = 1L) {

  if(!(identical(starting_position_index, 0L) || identical(starting_position_index, 1L)))
    stop("starting_position_index must be either 0L or 1L.")

  if(!is.character(chr))
    stop("chr needs to a character vector.")

  if(identical(length(chr), 0L))
    stop("chr is empty, must have at least one human chromosome name.")

  if(is.numeric(start) && !is.integer(start))
    stop("start needs to be an integer vector, append an \"L\" to the number.")

  if(!is.numeric(start) && !is.integer(start))
    stop("start needs to be an integer vector.")

  if(identical(length(start), 0L))
    stop("start is empty, must have at least one start position.")

  if(is.numeric(end) && !is.integer(end))
    stop("end needs to be an integer vector, append an \"L\" to the number.")

  if(!is.numeric(start) && !is.integer(end))
    stop("end needs to be an integer vector.")

  if(identical(length(end), 0L))
    stop("end is empty, must have at least one end position.")

  n_chr <- length(chr)
  n_start <- length(start)
  n_end <- length(end)
  if(! (identical(n_start, n_end) && identical(n_start, n_chr))) # identical(n_end, n_chr) == TRUE follows.
    stop("chr, start and end vectors should be of same length: ",
         "len(chr) = ", n_chr, ", ",
         "len(start) = ", n_start, ", and ",
         "len(end) = ", n_end, ".")

  valid_chr_names <- chr %in% chromosomes
  if(!all(valid_chr_names))
    stop("The following are not human chromosome names: ",
         concatenate::cc_and(quote(unique(chr[!valid_chr_names])), oxford = TRUE), ".")

  is_start_below_starting_pos <- start < starting_position_index
  if(any(is_start_below_starting_pos))
    stop("All start positions must be greater than ", starting_position_index, ", these are not: ",
         concatenate::cc_and(start[is_start_below_starting_pos], oxford = TRUE), ".")

  is_end_below_starting_pos <- end < starting_position_index
  if(any(is_end_below_starting_pos))
    stop("All end positions must be greater than ", starting_position_index, ", these are not: ",
         concatenate::cc_and(end[is_end_below_starting_pos], oxford = TRUE), ".")

  # Generate genomic ranges strings.
  gen_ranges <- sprintf("%s:%d-%d", chr, start, end)

  # When is start greater than end? (should not happen.)
  start_gr_end <- start > end
  if(any(start_gr_end))
    stop("start positions cannot be larger than end positions: ",
         concatenate::cc_and(gen_ranges[start_gr_end], oxford = TRUE), ".")

  # Check that all genomic ranges' strings conform to criteria of is_genomic_range.
  is_gen_ranges <- is_genomic_range(gen_ranges)
  if(!all(is_gen_ranges))
    stop("The following are not well-formed genomic ranges: ",
         concatenate::cc_and(gen_ranges[!is_gen_ranges], oxford = TRUE), ".")

  return(gen_ranges)

}

#' Filter a genomic location dataframe by chromosome name.
#'
#' Filter genomic location cases by chromosome name. This is useful to filter
#' out loci mapped to non-standard chromosomes, such as: \itemize{ \item GRC
#' assembly patches \item haplotype (HAPs) \item pseudo autosomal regions (PARs)
#' }
#' Only genomic locations lying in \code{chr_names_to_keep} are kept, and then, if more
#' than one is still present, we return only one, the first.
#'
#'
#' @param df A dataframe with the columns:
#' \itemize{
#' \item \code{chromosomeName}
#' \item \code{chromosomePosition}
#' \item \code{region.name}
#' \item \code{_links.snps.href}
#' }
#' @param chr_names_to_keep A character vector of human chromosome names.
#' @param warnings Whether to show warnings.
#'
#' @return A dataframe (a \code{\link[tibble]{tibble}} actually) with one row
#'   only, corresponding to a single genomic location.
#'
#' @keywords internal
filter_genomic_location_by_chr_name <- function(df, chr_names_to_keep = chromosomes, warnings = TRUE) {

  genomic_locations_variables <- c("chromosomeName",
                                   "chromosomePosition",
                                   "region.name",
                                   "_links.snps.href")

  if(!is.data.frame(df))
    stop("df needs to be a dataframe.")

  if(!(all(genomic_locations_variables %in% colnames(df))))
    stop("df must contain all of the following variables:\n",
         concatenate::cc_and(genomic_locations_variables),".")

  if(identical(nrow(df), 0L)) {
    if(warnings)
      warning("The dataframe df is empty. Filling in NAs...")

    # A one-row tibble filled with NA values.
    df2 <- tibble::tibble("chromosomeName" = NA_character_,
                          "chromosomePosition" = NA_integer_,
                          "region.name" = NA_character_,
                          "_links.snps.href" = NA_character_)
    return(df2)
  }

  # To appease R CMD check (not happy with this.)
  chromosomeName <- NULL
  # Filter genomic locations by the variable chromosomeName
  df2 <- dplyr::filter(df, chromosomeName %in% chr_names_to_keep)

  # If filtering resulted in an empty dataframe, just return it.
  if(identical(nrow(df2), 0L)) {
    if(warnings)
      warning("Filtering of genomic locations resulted in an empty dataframe!")

    # A one-row tibble filled with NA values.
    df2 <- tibble::tibble("chromosomeName" = NA_character_,
                          "chromosomePosition" = NA_integer_,
                          "region.name" = NA_character_,
                          "_links.snps.href" = NA_character_)
    return(df2)
  }

  # If only one genomic location is found, nice!,
  # that's how we expected it to be.
  if(identical(nrow(df2), 1L))
    return(tibble::as_tibble(df2))

  # If more than one location is found, err.. it's a bit strange as one SNP
  # should map only to one genomic location in one bona fide chromosome, so we
  # enforce it to be one location only and return the first row (ad hoc choice).
  if(nrow(df2) > 1L) {
    if(warnings)
      warning("Filtering of genomic locations did not result in one unique location!\n",
              "Picking the first, ad hoc.")
    return(tibble::as_tibble(df2[1, ]))
  }
}

#' Convert a SNP list object to a tibble.
#'
#' Converts the content returned by a request to either a
#' \code{"/snpLocation/{chr:start-end}"} or a
#' \code{"singleNucleotidePolymorphisms/{rsId}"} to a
#' \code{\link[tibble]{tibble}}.
#'
#' @param snp_content The response content (list).
#'
#' @return A \code{\link[tibble]{tibble}} where each row pertains a SNP, with
#'   the following columns: \itemize{ \item \code{rsId}, a \code{character}
#'   vector. \item \code{merged}, an \code{integer} vector. \item
#'   \code{chromosomeName}, a \code{character} vector. \item
#'   \code{chromosomePosition}, an \code{integer} vector. \item
#'   \code{region.name}, a \code{character} vector. \item
#'   \code{functionalClass}, a \code{character} vector. \item
#'   \code{lastUpdateDate}, a \code{character} vector. \item
#'   \code{genomicContexts}, a \code{list}. }
#'
#'
#' @keywords internal
snp_content_to_tibble <- function(snp_content) {

  snp <- snp_content
  locations <- snp$locations
  genomicContexts <- snp$genomicContexts

  snp$locations <- NULL
  snp$genomicContexts <- NULL
  snp$`_links` <- NULL

  snp_df <- tibble::as_tibble(snp)
  if(identical(class(locations), "data.frame"))
    location_df <- filter_genomic_location_by_chr_name(locations)
  else { # expecting a list of data frames
    location_df <- purrr::map_dfr(locations, filter_genomic_location_by_chr_name)
  }

  #genomic_contexts_df <- tibble::tibble(genomicContexts = list(drop_links(genomicContexts)))
  if(identical(class(genomicContexts), "data.frame"))
    genomic_contexts_df <- tibble::tibble(genomicContexts = list(drop_links(genomicContexts)))
  else { # expecting a list of data frames
    genomic_contexts_df <- tibble::tibble(genomicContexts = purrr::map(genomicContexts, drop_links))
  }


  #snp_df

  snp2 <- dplyr::bind_cols(snp_df, location_df, genomic_contexts_df)
  # Reorder columns
  snp3 <- dplyr::select(snp2, "rsId", "merged",
                        "chromosomeName", "chromosomePosition", "region.name",
                        "functionalClass", "lastUpdateDate", "genomicContexts")
  snp4 <- drop_links(snp3)

  return(snp4)

}


#' Retrieve SNPs by genomic location.
#'
#' Retrieve SNPs by genomic location. Genomic ranges are provided
#' through the three arguments: \code{chr}, \code{start}, and \code{end}. Genomic ranges
#' are assembled from these three vectors by matching elements by position. Thus,
#' all three vectors must have the same length.
#'
#' @param chr A character vector of human chromosome names.
#' @param start An integer vector of start positions.
#' @param end An integer vector of end positions.
#' @param verbose Whether to be chatty or not.
#' @param warnings Whether to print warnings or not.
#' @param drop_links Whether to drop columns related with links to resources.
#' @param query_grange Whether to keep an extra column of the genomic range
#'   query.
#' @param remove_duplicated_snps Whether to remove duplicated SNPs in output.
#' This can happen when querying several genomic intervals that overlap with
#' each other.
#'
#' @return A \code{\link[tibble]{tibble}} where rows are SNPs and columns are:
#'   \itemize{
#'   \item \code{query_grange}, the query genomic range (if
#'   \code{query_grange == TRUE});
#'   \item \code{rsId}, the SNP Id;
#'   \item \code{merged}, Whether this SNP has been merged with another SNP in a
#'   newer genome build;
#'   \item \code{chromosomeName}, the name of the chromosome where the SNP is
#'   located;
#'   \item \code{chromosomePosition}, the genomic position
#'   along the chromosome (1-based indexing); \item \code{region.name}, the
#'   genomic location of this SNP according to the
#'   \href{https://en.wikipedia.org/wiki/Locus_(genetics)}{cytogenetic banding
#'   nomenclature}; \item \code{functionalClass}, the SNP’s functional class;
#'   \item \code{lastUpdateDate}, the last date this SNP’s mapping information
#'   was updated;
#'   \item \code{genomicContexts}, the genomic contexts for this
#'   SNP, including upstream, downstream and mapped genes. This is a
#'   \code{\link[tibble]{tibble}} of genomic contexts for each SNP, containing the columns:
#'   \itemize{
#'   \item \code{isIntergenic} TODO.
#'   \item \code{isUpstream} TODO.
#'   \item \code{isDownstream} TODO.
#'   \item \code{distance} TODO.
#'   \item \code{source} TODO.
#'   \item \code{mappingMethod} TODO.
#'   \item \code{isClosestGene} TODO.
#'   \item \code{gene.geneName} TODO.
#'   \item \code{gene.entrezGeneIds} TODO.
#'   \item \code{gene.ensemblGeneIds} TODO.
#'   \item \code{location.chromosomeName} TODO.
#'   \item \code{location.chromosomePosition} TODO.
#'   \item \code{location.region.name} TODO.
#'   \item \code{location._links.snps.href} TODO.
#'   }
#'}
#'
#' @export
snps_by_location <- function(chr, start, end,
                             verbose = FALSE,
                             warnings = FALSE,
                             drop_links = TRUE,
                             query_grange = TRUE,
                             remove_duplicated_snps = FALSE) {

  # Generate genomic ranges' query strings
  genomic_ranges <- as_genomic_range(chr, start, end, starting_position_index = 1L)
  endpoint <- "/snpLocation"

  # Prepend the endpoint to assemble the resource URLs
  resource_urls <- sprintf("%s/%s", endpoint, genomic_ranges)

  # Request all SNPs by location
  responses <- purrr::map(resource_urls, request, verbose = verbose, warnings = warnings)
  is_valid_response <- purrr::map_lgl(responses, is_response_successful) & !purrr::map_lgl(responses, is_content_empty)

    # Only keep successful responses (code == 200).
    snp_content <- function(x, grange) {
      df <- snp_content_to_tibble(x$content$`_embedded`$singleNucleotidePolymorphisms)
      if(query_grange)
        df <- tibble::add_column(df, query_grange = grange, .before = TRUE)
      return(df)
    }
    snps <- purrr::map2_dfr(
      .x = responses[is_valid_response],
      .y = genomic_ranges[is_valid_response],
      .f = snp_content)

    if(identical(nrow(snps), 0L) && warnings)
      warning("No snps found within the queried genomic ranges.")

    # Because of multiple genomic range queries, of overlapping intervals
    # the same SNP may show up more than once. If remove_duplicated_snps = TRUE
    # we remove those duplicated entries (rows).
    if(remove_duplicated_snps) {
      rsId <- NULL # To appease R CMD check (not happy with this.)
      snps <- dplyr::distinct(snps, rsId, .keep_all = TRUE)
    }

    return(snps)
}

#' Is a string a valid rsID?
#'
#' Find which strings are valid SNP reference IDs, i.e., of the form rs[0-9]+.
#' Please note that this only does a syntax validation on the strings. It does
#' not check whether the actual IDs exist in dbSNP.
#'
#' @param str A character vector of putative SNP reference IDs of the form
#'   rs[0-9]+.
#' @param convert_NA_to_FALSE Whether to preserve \code{NA}
#'   (\code{convert_NA_to_FALSE = FALSE}) or whether to return \code{FALSE} when
#'   an \code{NA} is found (\code{convert_NA_to_FALSE = TRUE}).
#' @return Returns a logical vector of the same length as \code{str},
#'   \code{TRUE} for strings that are valid rs IDs, and \code{FALSE} otherwise.
#' @examples
#' # Check if strings are rsIDs
#' is_rs_id(c("rs123")) # TRUE
#'
#' # Test a vector of rsIDs
#' is_rs_id(
#'   c("rs123", "rs0001", "rs09123")) # TRUE TRUE TRUE
#'
#' # By default NAs are returned as they are.
#' is_rs_id(
#'   c("rs123", "rs0001", NA_character_)) # TRUE TRUE NA
#'
#' # Use the argument convert_NA_to_FALSE = TRUE to get FALSE instead of NA.
#' is_rs_id(
#'   c("rs123", "rs0001", NA_character_),
#'   convert_NA_to_FALSE = TRUE) # TRUE TRUE FALSE
#'
#' @export
is_rs_id <- function(str, convert_NA_to_FALSE = FALSE) {

  if(!is.character(str))
    stop("str argument must be a character vector.")

  if(identical(length(str), 0L))
    stop("str contains no values, it must contain at least one string.")

  # Replace NA with "".
  if(convert_NA_to_FALSE) {
    str2 <- str
    str2[is.na(str)] <- ""
  } else {
    str2 <- str
  }

  is_rs_id <- stringr::str_detect(str2, "^rs\\d+$")

  return(is_rs_id)
}

#' Get SNPs by rsId.
#'
#' Retrieve SNPs by Id.
#'
#' @param snp_ids SNP Ids, starting with "rs".
#' @param verbose Whether to be chatty or not.
#' @param warnings Whether to print warnings or not.
#' @param collapse Whether to bind all queries into one single dataframe.
#'
#' @return TODO.
#'
#' @examples
#'
#'
#' @export
# get_snps <- function(snp_ids) {
#
#
#
# }
