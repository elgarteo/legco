#' Voting Record in LegCo Meetings
#'
#' Fetch voting records of LegCo council, the Finance Committee and its
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
#' @param seperate_mechanism Only fetch votes that were counted with the vote
#'   seperate mechanism, i.e. requiring majority in both geographical and
#'   functional constituencies to pass. If `NULL`, returns all votes regardless
#'   of the vote counting mechanism used. Defaults to `NULL`.
#'
#' @param from Only fetch votes conducted at or after this time. Accepts
#'   character values in `'YYYY-MM-DDTHH:MM:SS'` format, and objects of class
#'   `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or anything else that can be coerced
#'   to a time with `as.POSIXlt()`. Defaults to `'1900-01-01T00:00:00'`.
#'
#' @param to Only fetch votes conducted at or before this time. Accepts character
#'   values in `'YYYY-MM-DDTHH:MM:SS'` format, and objects of class `Date`,
#'   `POSIXt`, `POSIXct`, `POSIXlt` or anything else that can be coerced to a
#'   time with `as.POSIXlt()`. Defaults to system time.
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
                          name_ch = NULL, seperate_mechanism = NULL, 
                          from = '1900-01-01T00:00:00', to = Sys.time(),
                          n = 10000, extra_param = NULL, verbose = TRUE) {
  query <- "vVotingResult?"
  
  filter_args <- {}
  
  if (!is.null(committee)) {
    committee <- capitalise(committee)
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
  
  if (!is.null(seperate_mechanism)) {
    if (seperate_mechanism) {
      filter_args <- c(filter_args, paste0("vote_separate_mechanism eq 'Yes'"))
    } else {
      filter_args <- c(filter_args, paste0("vote_separate_mechanism eq 'No'"))
    }
  }
  
  from <- convert_time(from)
  to <- convert_time(to)
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
