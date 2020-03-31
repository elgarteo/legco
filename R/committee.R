#' Committees of LegCo
#'
#' Fetch basic information of LegCo committees.
#'
#' This function corresponds to the \emph{Tcommittee} data endpoint of the
#' Meeting Schedule Database.
#'
#' @inherit meeting_schedule-db
#'
#' @examples
#' \donttest{
#' # Fetch all LegCo committees from the sixth term
#' x <- committee(term_id = 5)
#' }
#'
#' @export
#' 
committee <- function(committee_id = NULL, code = NULL, term_id = NULL, 
                      extra_param = NULL, count = FALSE, verbose = TRUE) {
  query <- "Tcommittee?$select=committee_id,committee_code,name_eng,name_chi,term_id,home_url_eng,home_url_chi"
  
  filter_args <- {}
  
  if (!is.null(committee_id)) {
    filter_args <- c(filter_args, generate_filter("committee_id", committee_id))
  }
  
  if (!is.null(code)) {
    code <- toupper(code)
    filter_args <- c(filter_args, generate_filter("committee_code", code))
  }
  
  if (!is.null(term_id)) {
    filter_args <- c(filter_args, generate_filter("term_id", term_id))
  }
  
  if(!is.null(filter_args)) {
    query <- paste0(query, "&$filter=", paste(filter_args, collapse = " and "))
  }
  
  if (!is.null(extra_param)) {
    query <- paste0(query, extra_param)
  }
  
  df <- legco_api("schedule", query, 10000, count, verbose)
  
  if (!count) {
    colnames(df) <- unify_colnames(colnames(df)) # in utils-misc.R
    df$CommitteeID <- sapply(df$CommitteeID, as.numeric)
  }
  
  df
}

#' @rdname committee
#' @export
legco_committee <- committee
