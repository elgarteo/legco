#' Committee of Meetings in LegCo
#'
#' Fetch all the meeting slots of a given committee(s), or the committee of a
#' given meeting slot(s).
#'
#' This function corresponds to the \emph{Tmeeting_committee} data endpoint of
#' the Meeting Schedule Database.
#'
#' @inherit meeting_schedule-db
#'
#' @examples
#' \donttest{
#' # Fetch all meetings of Subcommittee on Issues Relating to Bazaars
#' x <- meeting_committee(committee_id = 2704)
#' }
#'
#' @export
#' 
meeting_committee <- function(slot_id = NULL, meet_id = NULL, committee_id = NULL,
                              extra_param = NULL, count = FALSE, verbose = TRUE) {
  query <- "Tmeeting_committee?$select=committee_id,slot_id,Tmeeting&$expand=Tmeeting"
  
  filter_args <- {}
  
  if (!is.null(slot_id)) {
    filter_args <- c(filter_args, .generate_filter("slot_id", slot_id))
  }
  
  if (!is.null(committee_id)) {
    filter_args <- c(filter_args, .generate_filter("committee_id", committee_id))
  }
  
  if(!is.null(filter_args)) {
    query <- paste0(query, "&$filter=", paste(filter_args, collapse = " and "))
  }
  
  if (!is.null(extra_param)) {
    query <- paste0(query, extra_param)
  }
  
  df <- legco_api("schedule", query, 10000, count, verbose)
  
  if (!count) {
    colnames(df) <- .unify_colnames(colnames(df)) # in utils-misc.R
    colnames(df)[2] <- "SlotID"
    df$CommitteeID <- sapply(df$CommitteeID, as.numeric)
    df <- df[, c(1:2, 4:5, 8, 12:13, 9:11, 14, 6:7)]
  }
  
  df
}

#' @rdname meeting_committee
#' @export
legco_meeting_committee <- meeting_committee
