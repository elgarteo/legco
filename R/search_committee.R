#' Search Committee
#'
#' Search committee by full or partial name of committee.
#'
#' This is a complementary function to \code{\link{voting_record}()}.
#'
#' @param search_string search string of committee name. Accepts Chinese or
#'   English full or partial name. Defaults to \code{NULL}.
#'
#' @param exact logical: Whether to look for exact match of the search term.
#'   Defaults to \code{TRUE}.
#'
#' @inheritParams voting_record
#' @inheritParams meeting_schedule-db
#' @inherit comp-fun
#'
#' @export
#' 
search_committee <- function(search_string, term_id = NULL, exact = TRUE, verbose = TRUE) {
  df <- committee(verbose = verbose)
  # Detect language of input
  if (grepl("[^\001-\177]", search_string)) { 
    # If Chinese
    if (exact)
      search_tmp <- search_string
    else
      search_tmp <- strsplit(search_string, "")[[1]]
    index <- sapply(search_tmp, function(x) grep(x, df$NameChi))
  } else {
    # If English
    if (exact)
      search_tmp <- search_string
    else
      search_tmp <- strsplit(search_string, " ")[[1]]
    index <- sapply(search_tmp, function(x) grep(x, df$NameEng, ignore.case = TRUE))
  }
  index <- unique(unlist(index))
  df <- df[index, ]
  if (!is.null(term_id))
    df <- df[df$TermID %in% term_id, ]
  if (!nrow(df)) 
    stop("No matching result for search string \"", search_string, "\".")
  rownames(df) <- 1:nrow(df)
  
  if (verbose) 
    message(nrow(df), " record(s) match(es) your parameters.")
  df
}

#' @rdname search_committee
#' @export
legco_search_committee <- search_committee
