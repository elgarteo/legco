#' Search Voting Record
#'
#' Search voting record by conducted committee or date or meeting slot or LegCo
#' member(s). Note that only voting records from the Council, House Committee,
#' Finance Committee and its subcommittees are available.
#'
#' This is a complementary function to \code{\link{voting_record}()}.
#'
#' @param ... optional arguments passed to \code{\link{voting_record}()}.
#'
#' @inheritParams voting_record
#' @inheritParams hansard-db
#' @inheritParams meeting_schedule-db
#' @inherit comp-fun
#'
#' @export
#' 
search_voting_record <- function(speaker_id = NULL, member_id = NULL,
                                 committee_id = NULL, slot_id = NULL,
                                 from = "1900-01-01 00:00:00", to = Sys.time(),
                                 verbose = TRUE, ...) {
  members <- c()
  if (!is.null(speaker_id)) {
    tmp <- speakers(speaker_id = speaker_id, verbose = verbose)
    members <- c(members, tmp$NameChi)
  }
  if (!is.null(member_id)) {
    tmp <- member(member_id = member_id, verbose = verbose)
    tmp <- paste0(tmp$SurnameChi, tmp$FirstnameChi)
    members <- c(members, tmp)
  }
  if (!is.null(slot_id)) {
    if (length(slot_id) > 1)
      stop("Please enter only one Slot ID.")
    tmp <- meeting_committee(slot_id = slot_id, verbose = verbose)
    from <- as.Date(tmp$StartDateTime)
    to <- as.Date(tmp$StartDateTime)
    committee_id <- tmp$CommitteeID
  }
  if (!is.null(committee_id)) {
    tmp <- committee(committee_id = committee_id, verbose = verbose)
    committee_name <- tmp$NameEng
    committee_term <- tmp$TermID
  } else {
    committee_name <- NULL
    committee_term <- NULL
  }
  n <- voting_record(committee = committee_name, term_id = committee_term, name_ch = members,
                     from = from, to = to, count = TRUE, verbose = FALSE, ...)
  if (n == 0)
    stop("No matching result for given parameter.")
  df <- voting_record(committee = committee_name, term_id = committee_term, name_ch = members, 
                      from = from, to = to, verbose = verbose, n = n, ...)
  df <- df[, c(6, 3:4, 7:34)]
  if (verbose) 
    message(nrow(df), " record(s) match(es) your parameters.")
  df
}

#' @rdname search_voting_record
#' @export
legco_search_voting_record <- search_voting_record
