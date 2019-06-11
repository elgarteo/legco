#' Committee of Mettings in LegCo
#'
#' Fetch the committee in committee ID by using meet ID
#'
#' @param meet_id The id of a LegCo committee meeting, or a vector of ids. If `NULL`, returns
#'   all meetings. Defaults to `NULL`.
#'   
#' @param committee_id The id of a LegCo committee, or a vector of ids. If `NULL`, returns
#'   committees. Defaults to `NULL`.
#'
#' @param extra_param Additional query parameters defined in LegCo API. Must
#'   begin with `'&'`.
#'
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
meeting_committee <- function(meet_id = NULL, committee_id = NULL, extra_param = NULL, verbose = TRUE) {
  query <- "Tmeeting_committee?$select=meet_committee_id,committee_id"
  
  filter_args <- {}
  
  if (!is.null(meet_id)) {
    filter_args <- c(filter_args, generate_filter("meet_committee_id", meet_id))
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
  
  df <- legco_api("schedule", query, 1000, verbose)
  
  if (!is.null(df)) {
    colnames(df) <- unify_colnames(colnames(df)) # in utils-misc.R
    colnames(df)[1] <- "MeetId"
    
    df
  }
}

#' @rdname meeting_committee
#' @export
legco_meeting_committee <- meeting_committee
