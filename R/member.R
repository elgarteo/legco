#' Members of LegCo
#'
#' Fetch the basic information of LegCo members.
#'
#' This function corresponds to the \emph{Tmember} data endpoint of the Meeting
#' Schedule Database.
#'
#' @inherit meeting_schedule-db
#'
#' @examples
#' \donttest{
#' # Fetch full list of members
#' x <- member()
#' }
#'
#' @export
#' 
member <- function(member_id = NULL, extra_param = NULL, count = FALSE, verbose = TRUE) {
  query <- "Tmember?$select=member_id,title_eng,surname_eng,firstname_eng,english_name,honourable_eng,surname_chi,firstname_chi,title_chi,honourable_chi,latest_term_id"
  
  filter_args <- {}
  
  if (!is.null(member_id)) {
    filter_args <- c(filter_args, generate_filter("member_id", member_id))
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
  }
  
  df
}

#' @rdname member
#' @export
legco_member <- member
