#' Extract association identifiers from URLs
#'
#' This function extracts association identifiers from URLs of the form:
#' \code{".*{association_id}$"}.
#'
#' @param urls A character vector of URLs of the form
#'   \code{".*{association_id}$"}.
#'
#' @return A character vector of association identifiers.
#' @keywords internal
extract_association_id <- function(urls) {
  assoc_ids <- stringr::str_extract(urls, "(\\d+)$")
  return(assoc_ids)
}

#' Extract allele names from strings of the form rs123-G
#'
#' This function parses strings of the form \code{"rs123-G"} and returns
#' the name of the allele; it uses the regex \code{([ATCG]+)$}.
#'
#' @param risk_allele_names
#'
#' @return A character vector of allele names.
#'
#' @keywords internal
allele_name <- function(risk_allele_names) {
  str <- stringr::str_extract(risk_allele_names, "-([ATCG]+)$") %>%
    stringr::str_replace("-", "")
  str[is_empty_str(str)] <- NA_character_
  return(str)
}

#' Extract variant identifiers from strings of the form rs123-G
#'
#' This function parses strings of the form \code{"rs123-G"} and returns
#' the name of the variant; it uses the regex \code{-([ATCG\\?]+)?$}.
#'
#' @param risk_allele_names
#'
#' @return A character vector of variant names.
#'
#' @keywords internal
variant_name <- function(risk_allele_names) {
  str <- stringr::str_replace(risk_allele_names, "-([ATCG\\?]+)?$", "")
  str[is_empty_str(str)] <- NA_character_
  return(str)
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
#'   (\code{convert_NA_to_FALSE = TRUE}) or whether to return \code{FALSE} when
#'   an \code{NA} is found (\code{convert_NA_to_FALSE = FALSE}).
#' @return Returns a logical vector of the same length as \code{str},
#'   \code{TRUE} for strings that are valid rs IDs, and \code{FALSE} otherwise.
#' @keywords internal
is_rs_id <- function(str, convert_NA_to_FALSE = TRUE) {

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

  is_rs_id <- str_detect(str2, "^rs\\d+$")

  return(is_rs_id)
}

#' Is a string a GWAS Catalog association accession ID?
#'
#' Find which strings are valid GWAS Catalog association IDs (returns
#' \code{TRUE}). Association IDs are tested against the following regular
#' expression: \code{^\\\\d+$}.
#'
#' @param str A character vector of strings.
#' @param convert_NA_to_FALSE Whether to treat \code{NA} as \code{NA}
#'   (\code{convert_NA_to_FALSE = FALSE}) or whether to return \code{FALSE} when
#'   an \code{NA} is found (\code{convert_NA_to_FALSE = TRUE}).
#' @return A logical vector.
#' @keywords internal
is_association_id <- function(str, convert_NA_to_FALSE = TRUE) {
  if (!is.character(str))
    stop("str argument must be a character vector.")

  if (identical(length(str), 0L))
    stop("str contains no values, it must contain at least one string.")

  # Replace NA with "".
  if (convert_NA_to_FALSE) {
    str2 <- str
    str2[is.na(str)] <- ""
  } else {
    str2 <- str
  }

  is_accession <- str_detect(str2, "^\\d+$")

  return(is_accession)
}

#' Is a string a human chromosome name?
#'
#' Find which strings are valid human chromosome names. The valid chromosome
#' names can be specified via the argument \code{chromosomes}.
#'
#' @param string A character vector of strings.
#' @param chromosomes A character vector of valid chromosome names. Default is
#'   autosomal chromosomes 1 thru 22 and, X, Y, and MT.
#' @param convert_NA_to_FALSE Whether to treat \code{NA} as \code{NA}
#'   (\code{convert_NA_to_FALSE = FALSE}) or whether to return \code{FALSE} when
#'   an \code{NA} is found (\code{convert_NA_to_FALSE = TRUE}).
#' @return A logical vector.
#' @keywords internal
is_human_chromosome <- function(string,
                                chromosomes = c(seq_len(22), "X", "Y", "MT"),
                                convert_NA_to_FALSE = TRUE) {

  # Replace NA with "".
  if (convert_NA_to_FALSE) {
    string2 <- string
    string2[is.na(string)] <- ""
  } else {
    string2 <- string
  }

  chrom_regex <- stringr::str_c(chromosomes, collapse = "|")
  regex <- sprintf("^(%s)$", chrom_regex)
  return(str_detect(string2, regex))
}

#' Is a string an EFO trait ID?
#'
#' Find which strings are valid EFO trait IDs (returns
#' \code{TRUE}). EFO trait IDs are tested against the following regular
#' expression: \code{^EFO_\\\\d\{7\}$}.
#'
#' @param str A character vector of strings.
#' @param convert_NA_to_FALSE Whether to treat \code{NA} as \code{NA}
#'   (\code{convert_NA_to_FALSE = FALSE}) or whether to return \code{FALSE} when
#'   an \code{NA} is found (\code{convert_NA_to_FALSE = TRUE}).
#' @return A logical vector.
#' @keywords internal
is_efo_id <- function(str, convert_NA_to_FALSE = TRUE) {
  if (!is.character(str))
    stop("str argument must be a character vector.")

  if (identical(length(str), 0L))
    stop("str contains no values, it must contain at least one string.")

  # Replace NA with "".
  if (convert_NA_to_FALSE) {
    str2 <- str
    str2[is.na(str)] <- ""
  } else {
    str2 <- str
  }

  is_efo_trait <- str_detect(str2, "^EFO_\\d{7}$")

  return(is_efo_trait)
}

#' Is a string an EFO trait ID in the broad sense?
#'
#' This function is more permissible than \code{\link[gwasrapidd]{is_efo_id}}.
#' This function matches EFO trait IDs against the following regular expression:
#' \code{^\\\\w+$}. This is very forgiving on the input, any sequence of word
#' characters are ok. This is useful to match EFO identifiers that do not follow
#' the regex \code{^EFO_\\\\d\{7\}$}, such as: \code{'GO_0097334'},
#' \code{'HP_0001268'}, \code{'Orphanet_182098'}, and \code{'NCIT_C74532'}.
#'
#' @param str A character vector of strings.
#' @param convert_NA_to_FALSE Whether to treat \code{NA} as \code{NA}
#'   (\code{convert_NA_to_FALSE = FALSE}) or whether to return \code{FALSE} when
#'   an \code{NA} is found (\code{convert_NA_to_FALSE = TRUE}).
#' @return A logical vector.
#' @keywords internal
is_efo_id2 <- function(str, convert_NA_to_FALSE = TRUE) {
  if (!is.character(str))
    stop("str argument must be a character vector.")

  if (identical(length(str), 0L))
    stop("str contains no values, it must contain at least one string.")

  # Replace NA with "".
  if (convert_NA_to_FALSE) {
    str2 <- str
    str2[is.na(str)] <- ""
  } else {
    str2 <- str
  }

  is_efo_trait <- str_detect(str2, "^\\w+$")

  return(is_efo_trait)
}

#' Is a string a GWAS Catalog study accession ID?
#'
#' Find which strings are valid GWAS Catalog study accession IDs (returns
#' \code{TRUE}). Study accession IDs are tested against the following regular
#' expression: \code{^GCST\\\\d+$}.
#'
#' @param str A character vector of strings.
#' @param convert_NA_to_FALSE Whether to treat \code{NA} as \code{NA}
#'   (\code{convert_NA_to_FALSE = FALSE}) or whether to return \code{FALSE} when
#'   an \code{NA} is found (\code{convert_NA_to_FALSE = TRUE}).
#' @return A logical vector.
#' @keywords internal
is_study_id <- function(str, convert_NA_to_FALSE = TRUE) {
  if (!is.character(str))
    stop("str argument must be a character vector.")

  if (identical(length(str), 0L))
    stop("str contains no values, it must contain at least one string.")

  # Replace NA with "".
  if (convert_NA_to_FALSE) {
    str2 <- str
    str2[is.na(str)] <- ""
  } else {
    str2 <- str
  }

  is_accession <- str_detect(str2, "^GCST\\d+$")

  return(is_accession)
}

#' Is a string a PubMed ID?
#'
#' Find which strings are valid PubMed IDs (returns \code{TRUE}). PubMed IDs are
#' tested against the following regular expression: \code{^\\\\d+$}.
#'
#' @param str A character vector of strings.
#' @param convert_NA_to_FALSE Whether to treat \code{NA} as \code{NA}
#'   (\code{convert_NA_to_FALSE = FALSE}) or whether to return \code{FALSE} when
#'   an \code{NA} is found (\code{convert_NA_to_FALSE = TRUE}).
#' @return A logical vector.
#' @keywords internal
is_pubmed_id <- function(str, convert_NA_to_FALSE = TRUE) {

  if (!is.character(str))
    stop("str argument must be a character vector.")

  if (identical(length(str), 0L))
    stop("str contains no values, it must contain at least one string.")

  # Replace NA with "".
  if (convert_NA_to_FALSE) {
    str2 <- str
    str2[is.na(str)] <- ""
  } else {
    str2 <- str
  }

  is_pubmed_id <- str_detect(str2, "^\\d+$")

  return(is_pubmed_id)
}


#' Does a string contain a question mark?
#'
#' Find which strings contain a question mark. This function uses the following
#' regular expression: \code{[\\?]}.
#'
#' @param str A character vector of strings.
#' @param convert_NA_to_FALSE Whether to treat \code{NA} as \code{NA}
#'   (\code{convert_NA_to_FALSE = FALSE}) or whether to return \code{FALSE} when
#'   an \code{NA} is found (\code{convert_NA_to_FALSE = TRUE}).
#' @return A logical vector.
#' @keywords internal
contains_question_mark <- function(str, convert_NA_to_FALSE = TRUE) {

  if (!is.character(str))
    stop("str argument must be a character vector.")

  if (identical(length(str), 0L))
    stop("str contains no values, it must contain at least one string.")

  # Replace NA with "".
  if (convert_NA_to_FALSE) {
    str2 <- str
    str2[is.na(str)] <- ""
  } else {
    str2 <- str
  }

  contains_question_mark <- str_detect(str2, "[\\?]")
  return(contains_question_mark)
}

#' Is string empty or an all whitespace string?
#'
#' Matches the \code{string} vector against \code{"^\\\s*$"}.
#'
#' @param str A \code{\link[base]{character}} vector.
#' @param convert_NA_to_FALSE Whether to treat \code{NA} as \code{NA}
#'   (\code{convert_NA_to_FALSE = FALSE}) or whether to return \code{FALSE} when
#'   an \code{NA} is found (\code{convert_NA_to_FALSE = TRUE}).
#' @return A \code{\link[base]{logical}}.
#'
#' @keywords internal
is_empty_str <- function(str, convert_NA_to_FALSE = TRUE) {
  if (!is.character(str))
    stop("str argument must be a character vector.")

  if (identical(length(str), 0L))
    stop("str contains no values, it must contain at least one string.")

  # Replace NA with ".", i.e. a non-empty string.
  if (convert_NA_to_FALSE) {
    str2 <- str
    str2[is.na(str)] <- "."
  } else {
    str2 <- str
  }
  str_detect(str2, "^\\s*$")
}

#' Convert a cytogenetic band string to genomic coordinates.
#'
#' This function uses the provided \code{\link[gwasrapidd]{cytogenetic_bands}}
#' dataframe to convert cytogenetic band names to genomic coordinates.
#'
#' @param bands A \code{\link[base]{character}} vector of cytogenetic bands.
#'
#' @return A dataframe of genomic ranges. Columns are: chromosome, start and
#'   end. Each row corresponds to a queried cytogenetic band (in the same order
#'   as queried).
#' @keywords internal
cytogenetic_band_to_genomic_range <- function(bands) {
  if (!is.character(bands))
    stop("bands argument must be a character vector.")

  if (identical(length(bands), 0L))
    stop("bands contains no values, it must contain at least one string.")

  # Is it a valid existing human cytogenetic band?
  is_band <- bands %in% cytogenetic_bands$cytogenetic_band

  if(any(!is_band))
    stop("These are not valid cytogenetic bands: ",
         cc_and(bands[!is_band], oxford = TRUE), ".\n",
         "Check `cytogenetic_bands` dataframe for valid names.")

  cytogenetic_band <- rlang::expr(cytogenetic_band)
  chromosome <- rlang::expr(chromosome)
  start <- rlang::expr(start)
  end <- rlang::expr(end)

  # This alternative to dplyr::filter(cytogenetic_bands, cytogenetic_band %in%
  # bands) preserves order of bands in final output.
  genomic_ranges <-
    purrr::map_dfr(
      bands,
      ~ dplyr::filter(cytogenetic_bands, !!cytogenetic_band %in% .x)) %>%
    dplyr::select(!!chromosome, !!start, !!end)

  return(genomic_ranges)
}

#' Trim whitespace.
#'
#' This function removes leading and trailing white space from strings. Note:
#' this function does no checking on input for performance reasons. So make sure
#' the input is really a character vector.
#'
#'
#' @param x A \code{\link[base]{character}} vector.
#' @return A character vector.
#' @keywords internal
tws <- function (x) stringr::str_trim(x)
