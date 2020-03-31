#' Voting Record in LegCo Meetings
#'
#' Fetch voting records of LegCo council, the Finance Committee and its
#' subcommittees and the House Committee meetings.
#'
#' This function corresponds to the \emph{vVotingResult} data endpoint of the
#' Voting Result Database.
#'
#' @param committee the name of the committee or subcommittee. Defaults to
#'   \code{NULL}.
#'
#' @param result the voting result. If \code{"passed"}, returns motions that
#'   have been passed. If \code{"vetoed"}, returns motions that have been
#'   vetoed.If \code{"all"}, returns all motions that has been voted in LegCo.
#'   Defaults to \code{all}.
#'
#' @param vote the vote cast. If \code{"yes"}, returns only members who cast
#'   affirmative votes. If \code{"no"}, returns only members who cast negative
#'   votes. If \code{"abstain"}, returns only members who abstained from voting.
#'   If \code{"absent"}, returns only members who were absent. If
#'   \code{"present"}, returns only members who were present and did not vote
#'   (e.g. President). If \code{"all"}, returns all votes. Defaults to
#'   \code{"all"}.
#'
#' @param name_ch the name of a LegCo member in Traditional Chinese, or a vector
#'   of names. If \code{NULL}, returns voting records of all members. Defaults
#'   to \code{NULL}.
#'
#' @param name_en the name of a LegCo member in English, or a vector of names.
#'   If the member has an English name, the English name should go first
#'   followed by the surname in capital letters, e.g. \code{"Peter CHAN"}. If
#'   the member does not have an English name, the surname in capital letters
#'   should go first followed by the translated first name with a hyphen
#'   separating the different syllable, e.g. \code{"CHAN Tai-man"}. Check the
#'   names of the members with \code{\link{member}()}. If \code{NULL}, returns
#'   voting records of all members. Defaults to \code{NULL}.
#'
#' @param separate_mechanism only fetch votes that were counted with the vote
#'   separate mechanism, i.e. requiring majority in both geographical and
#'   functional constituencies to pass. If \code{NULL}, returns all votes
#'   regardless of the vote counting mechanism used. Defaults to \code{NULL}.
#'
#' @param mover_type the type of motion being put on vote. If \code{"PO"},
#'   returns votes on government motions only. If \code{"MB"}, returns votes on
#'   members' motions only. If \code{"all"}, returns votes on all motions.
#'   Defaults to \code{"all"}.
#'
#' @param from only fetch results of meetings on or after this date and time.
#'   Accepts character values in \code{"YYYY-MM-DD"} or \code{"YYYY-MM-DD
#'   HH:MM:SS"} format, and objects of class \code{Date}, \code{POSIXt},
#'   \code{POSIXct}, \code{POSIXlt} or anything else that can be coerced to a
#'   date with \code{as.Date()}. Defaults to \code{"1900-01-01 00:00:00"}.
#'
#' @param to only fetch results of meetings on or before this date and time.
#'   Accepts character values in \code{"YYYY-MM-DD"} or \code{"YYYY-MM-DD
#'   HH:MM:SS"} format, and objects of class \code{Date}, \code{POSIXt},
#'   \code{POSIXct}, \code{POSIXlt} or anything else that can be coerced to a
#'   date with \code{as.Date()}. Defaults to the current system time.
#'
#' @inheritParams hansard-db
#' @inheritParams term
#'
#' @seealso LegCo API documentation for the Voting Record database:
#'   \url{https://www.legco.gov.hk/odata/english/vrdb.html}
#'
#' @examples
#' \donttest{
#' # Fetch how members voted the motion on
#' # Abolishing the MPF Offsetting Mechanism on November 11, 2016
#' x <- voting_record(committee = "Council Meeting",
#'                    from = "2016-11-16 13:51:53",
#'                    to = "2016-11-16 13:51:53")
#' }
#'
#' @export
#' 
voting_record <- function(committee = NULL, term_id = NULL, result = "all",
                          vote = "all", name_ch = NULL, name_en = NULL,
                          separate_mechanism = NULL, mover_type = "all",
                          from = '1900-01-01 00:00:00', to = Sys.time(),
                          n = 10000, extra_param = NULL,
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
  
  if (!is.null(name_en)) {
    filter_args <- c(filter_args, generate_filter("name_en", name_en))
  }
  
  if (!is.null(separate_mechanism)) {
    if (separate_mechanism) {
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
  
  from <- posixlt2net(from)
  to <- posixlt2net(to)
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
