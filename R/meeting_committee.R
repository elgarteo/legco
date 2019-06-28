#' Committee of Meetings in LegCo
#'
#' Fetch the corresponding committee of meetings in LegCo, or vice versa.
#' Essentially a function to match Committee ID and Meet ID.
#'
#' @param slot_id The id of a LegCo meeting slot, or a vector of ids. If `NULL`,
#'   returns all meetings. Defaults to `NULL`.
#'
#' @param committee_id The id of a LegCo committee, or a vector of ids. If
#'   `NULL`, returns committees. Defaults to `NULL`.
#'
#' @param extra_param Additional query parameters defined in LegCo API. Must
#'   begin with `'&'`.
#'
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
meeting_committee <- function(slot_id = NULL, committee_id = NULL, extra_param = NULL, verbose = TRUE) {
  query <- "Tmeeting_committee?$select=committee_id,slot_id"
  
  filter_args <- {}
  
  if (!is.null(slot_id)) {
    filter_args <- c(filter_args, generate_filter("slot_id", slot_id))
  }
  
  if (!is.null(committee_id)) {
    filter_args <- c(filter_args, generate_filter("committee_id", committee_id))
  }
  
  if(!is.null(filter_args)) {
    query <- paste0(query, "&$filter=", paste(filter_args, collapse = " and "))
  }
  
  if (!is.null(extra_param)) {
    query <- paste0(query, extra_param)
  }
  
  df <- legco_api("schedule", query, 10000, verbose)
  
  if (!is.null(df)) {
    colnames(df) <- unify_colnames(colnames(df)) # in utils-misc.R
    colnames(df)[2] <- "MeetID"
    
    df
  }
}

#' @rdname meeting_committee
#' @export
legco_meeting_committee <- meeting_committee
