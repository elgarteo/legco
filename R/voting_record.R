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
#' @param result The voting result. If `'passed'`, returns motions that have
#'   been passed. If `'vetoed'`, returns motions that have been vetoed.If
#'   `'all'`, returns all motions that has been voted in LegCo. Defaults to
#'   `all`.
#'
#' @param vote The vote that casted. If `'yes'`, returns only members who casted
#'   affirmative votes. If `'no'`, returns only members who casted negative
#'   votes. If `'abstain'`, returns only members who abstained from voting. If
#'   `'absent'`, returns only members who were absent. If `'present'`, returns
#'   only members who were present and did not vote (e.g. President). If
#'   `'all'`, returns all votes. Defaults to `'all'`.
#'
#' @param name_ch The name of a LegCo member, or a vector of names. If `NULL`,
#'   returns voting records of all members. Defaults to `NULL`.
#'
#' @param seperate_mechanism Only fetch votes that were counted with the vote
#'   seperate mechanism, i.e. requiring majority in both geographical and
#'   functional constituencies to pass. If `NULL`, returns all votes regardless
#'   of the vote counting mechanism used. Defaults to `NULL`.
#'
#' @param mover_type The type of motion being put on vote. If `'PO'`, returns
#'   votes on government motions only. If `'MB'`, returns votes on members'
#'   motions only. If `'all'`, returns votes on all motions. Defaults to
#'   `'all'`.
#'
#' @param from Only fetch votes conducted at or after this time. Accepts
#'   character values in `'YYYY-MM-DDTHH:MM:SS'` format, and objects of class
#'   `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or anything else that can be coerced
#'   to a time with `as.POSIXlt()`. Defaults to `'1900-01-01T00:00:00'`.
#'
#' @param to Only fetch votes conducted at or before this time. Accepts
#'   character values in `'YYYY-MM-DDTHH:MM:SS'` format, and objects of class
#'   `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or anything else that can be coerced
#'   to a time with `as.POSIXlt()`. Defaults to system time.
#'
#' @inheritParams legco_api
#' @inheritParams hansard
#'
#' @export
#' 
voting_record <- function(committee = NULL, term_id = NULL, result = "all",
                          vote = "all", name_ch = NULL, seperate_mechanism = NULL,
                          mover_type = "all", from = '1900-01-01T00:00:00',
                          to = Sys.time(), n = 10000, extra_param = NULL,
                          count = FALSE, verbose = TRUE) {
  query <- "vVotingResult?"
  
  filter_args <- {}
  
  if (!is.null(committee)) {
    committee <- capitalise(committee)
    filter_args <- c(filter_args, paste0("type eq '", committee, "'"))
  }
  
  if (!is.null(term_id)) {
    term_id <- convert_term_id(term_id)
    filter_args <- c(filter_args, paste0("term_no eq ", term_id))
  }
  
  results <- tolower(result)
  if (result == "passed") {
    filter_args <- c(filter_args, "overall_result eq 'Passed'")
  } else if (result == "vetoed") {
    filter_args <- c(filter_args, "overall_result eq 'Negatived'")
  }
  
  vote <- tolower(vote)
  if (vote == "yes") {
    filter_args <- c(filter_args, "vote eq 'Yes'")
  } else if (vote == "no") {
    filter_args <- c(filter_args, "vote eq 'No'")
  } else if (vote == "abstain") {
    filter_args <- c(filter_args, "vote eq 'Abstain'")
  } else if (vote == "absent") {
    filter_args <- c(filter_args, "vote eq 'Absent'")
  } else if (vote == "present") {
    filter_args <- c(filter_args, "vote eq 'Present'")
  }
  
  if (!is.null(name_ch)) {
    filter_args <- c(filter_args, generate_filter("name_ch", name_ch))
  }
  
  if (!is.null(seperate_mechanism)) {
    if (seperate_mechanism) {
      filter_args <- c(filter_args, "vote_separate_mechanism eq 'Yes'")
    } else {
      filter_args <- c(filter_args, "vote_separate_mechanism eq 'No'")
    }
  }
  
  mover_type <- toupper(mover_type)
  if (mover_type == "PO") {
    filter_args <- c(filter_args, "mover_type eq 'Public Officer'")
  } else if (mover_type == "MB") {
    filter_args <- c(filter_args, "mover_type eq 'Member'")
  }
  
  from <- convert_time(from)
  to <- convert_time(to)
  if (from == to & grepl("T00:00:00", from) & grepl("T00:00:00", to)) {
    to <- gsub("T.*", "T23:59:59", to)
  }
  filter_args <- c(filter_args, paste0("vote_time ge datetime\'", from, 
                                       "\' and vote_time le datetime\'", to, "\'"))
  
  query <- paste0(query, "$filter=", paste(filter_args, collapse = " and "))
  
  if (!is.null(extra_param)) {
    query <- paste0(query, extra_param)
  }
  
  df <- legco_api("vrdb/odata", query, n, count, verbose)
  
  if (!count) {
    # Rename column names
    colnames(df) <- unify_colnames(colnames(df)) # in utils-misc.R
    names(df)[names(df) == "TermNo"] <- "TermID"
    names(df)[names(df) == "Type"] <- "Committee"
    df$TermID <- sapply(df$TermID, convert_term_no)
    df <- df[, c(1, 2:4, 6:36)]
  }
  
  df
}

#' @rdname voting_record
#' @export
legco_voting_record <- voting_record
