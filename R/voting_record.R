#' Voting Record in LegCo Meetings
#'
#' Fetch voting record from LegCo Council, the Finance Committee and its
#' subcommittees and the House Committee meetings.
#'
#' @param committee The name of the committee or subcommittee. Defaults to
#'   `NULL`.
#'   
#' @param term_id The ID of a term. Defaults to `NULL`.
#'
#' @param result The voting result. `'passed'` returns motions that have been
#'   passed. `'vetoed'` returns motions that have been vetoed. `'all'` returns
#'   all motions that has been voted in LegCo. Defaults to `all`.
#'
#' @param name_ch The name of a LegCo member, or a vector of names. If `NULL`,
#'   returns voting records  of all members. Defaults to `NULL`.
#'
#' @param from Only fetch results of meetings on or after this date. Accepts
#'   character values in `'YYYY-MM-DD'` format, and objects of class `Date`,
#'   `POSIXt`, `POSIXct`, `POSIXlt` or anything else that can be coerced to a
#'   date with `as.Date()`. Defaults to `'1900-01-01'`.
#'
#' @param to Only fetch results of meetings on or before this date. Accepts
#'   character values in `'YYYY-MM-DD'` format, and objects of class `Date`,
#'   `POSIXt`, `POSIXct`, `POSIXlt` or anything else that can be coerced to a
#'   date with `as.Date()`. Defaults to the current system date.
#'
#' @param n The number of records to request. Defaults to `10000`.
#'
#' @param extra_param Additional query parameters defined in LegCo API. Must
#'   begin with `'&'`.
#'
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
voting_record <- function(committee = NULL, term_id = NULL, result = "all",
                          name_ch = NULL, from = '1900-01-01', to = Sys.Date(),
                          n = 10000, extra_param = NULL, verbose = TRUE) {
  query <- "vVotingResult?"
  
  filter_args <- {}
  
  if (!is.null(committee)) {
    committee <- strsplit(committee, " ")[[1]]
    committee <- paste0(toupper(substr(committee, 1, 1)), substr(committee, 2, nchar(committee)))
    committee <- paste(committee, collapse = " ")
    filter_args <- c(filter_args, paste0("type eq '", committee, "'"))
  }
  
  if (!is.null(term_id)) {
    filter_args <- c(filter_args, paste0("term_no eq ", term_id))
  }
  
  results <- tolower(result)
  if (result == "passed") {
    filter_args <- c(filter_args, paste0("VoteResults eq 'Passed'"))
  } else if (result == "vetoed") {
    filter_args <- c(filter_args, paste0("VoteResults eq 'Negatived'"))
  }
  
  if (!is.null(name_ch)) {
    filter_args <- c(filter_args, generate_filter("name_ch", name_ch))
  }
  
  from <- as.Date(from)
  from <- paste0(format(from, "%Y-%m-%d"), "T00:00:00")
  to <- as.Date(to)
  to <- paste0(format(to, "%Y-%m-%d"), "T23:59:59")
  filter_args <- c(filter_args, paste0("vote_time ge datetime\'", from, 
                                       "\' and vote_time le datetime\'", to, "\'"))
  
  query <- paste0(query, "$filter=", paste(filter_args, collapse = " and "))
  
  if (!is.null(extra_param)) {
    query <- paste0(query, extra_param)
  }
  
  df <- legco_api("vrdb/odata", query, n, verbose)
  
  if (!is.null(df)) {
    # Rename column names
    colnames(df) <- unify_colnames(colnames(df)) # in utils-misc.R
    names(df)[names(df) == "TermNo"] <- "TermID"
    names(df)[names(df) == "Type"] <- "Committee"
    
    df
  }
}

#' @rdname voting_record
#' @export
legco_voting_record <- voting_record
