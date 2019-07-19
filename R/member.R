#' Members of LegCo
#'
#' Fetch the basic information of LegCo members.
#'
#' @param id The id of a LegCo member, or a vector of ids. If `NULL`, returns
#'   all LegCo members. Defaults to `NULL`.
#'
#' @param extra_param Additional query parameters defined in LegCo API. Must
#'   begin with `'&'`.
#'   
#' @param count If `TRUE`, returns only the total count of records that matches
#'   the paramter(s) instead of the result. Defaults to `FALSE`.
#'
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
member <- function(id = NULL, extra_param = NULL, count = FALSE, verbose = TRUE) {
  query <- "Tmember?$select=member_id,title_eng,surname_eng,firstname_eng,english_name,honourable_eng,surname_chi,firstname_chi,title_chi,honourable_chi,latest_term_id"
  
  filter_args <- {}
  
  if (!is.null(id)) {
    filter_args <- c(filter_args, generate_filter("member_id", id))
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
