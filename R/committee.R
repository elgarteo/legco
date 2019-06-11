#' Committees of LegCo
#'
#' Fetch information of LegCo committees.
#'
#' @param id The id of a committee, or a vector of ids. If `NULL`, returns all
#'   committees. Defaults to `NULL`.
#'
#' @param code The code of a committee, or a vector of ids. If `NULL`, returns
#'   all committees. Defaults to `NULL`.
#'
#' @param term_id The id of a term, or a vector of ids. If `NULL`, returns
#'   committees of all terms.
#'
#' @param lang The language. `'en'` returns result in English. `'zh'` returns
#'   result in Traditional Chinese. Defaults to `'en'`.
#'
#' @param extra_param Additional query parameters defined in LegCo API. Must
#'   begin with `'&'`.
#'
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
committee <- function(id = NULL, code = NULL, term_id = NULL, 
                      extra_param = NULL, verbose = TRUE) {
  query <- "Tcommittee?$select=committee_id,committee_code,name_eng,name_chi,term_id,home_url_eng,home_url_chi"
  
  filter_args <- {}
  
  if (!is.null(id)) {
    filter_args <- c(filter_args, generate_filter("committee_id", id))
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
  
  df <- legco_api("schedule", query, 1000, verbose)
  
  if (!is.null(df)) {
    colnames(df) <- unify_colnames(colnames(df)) # in utils-misc.R
  
    df
  }
}

#' @rdname committee
#' @export
legco_committee <- committee
