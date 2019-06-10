#' Terms of Members of LegCo
#'
#' Fetch the serving terms of LegCo members
#'
#' @param id The id of a LegCo member, or a vector of ids. If `NULL`, returns
#'   all LegCo members. Defaults to `NULL`.
#'
#' @param extra_param Additional query parameters defined in LegCo API. Must
#'   begin with `'&'`.
#'
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
member_term <- function(id = NULL, extra_param = NULL, verbose = TRUE) {
  query <- "Tmember_term?$select=member_id,term_id"
  
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
  
  df <- legco_api("schedule", query, 1000, verbose)
  
  colnames(df) <- unify_colnames(colnames(df)) # in utils-misc.R
  
  # COMBINE 
  # 1st Step:
  #tmp <- sapply(1:sum(duplicated(test$MemberId)), function(x) {
  #paste(test$TermId[test$MemberId %in% test$MemberId[duplicated(test$MemberId)] & !duplicated(test$MemberId)][x], 
  #      test$TermId[duplicated(test$MemberId)][x])
  #})
  #2nd Step:
  #...
  
  df
  
}

#' @rdname member
#' @export
legco_member <- member
