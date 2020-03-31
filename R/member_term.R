#' Terms of Members of LegCo
#'
#' Fetch the serving terms in Term ID of LegCo members
#'
#' This function corresponds to the \emph{Tmember_term} data endpoint of the
#' Meeting Schedule Database.
#'
#' @inherit meeting_schedule-db
#'
#' @examples
#' \donttest{
#' #Fetches the term served by Hon Chan Kin-por and Kwong Chun-yu
#' x <- member_term(member_id = c(273, 924))
#' }
#'
#' @export
#' 
member_term <- function(member_id = NULL, extra_param = NULL, count = FALSE, verbose = TRUE) {
  query <- "Tmember_term?$select=member_id,term_id"
  
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

#' @rdname member_term
#' @export
legco_member_term <- member_term
