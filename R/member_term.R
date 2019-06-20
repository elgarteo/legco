#' Terms of Members of LegCo
#'
#' Fetch the serving terms in term ID of LegCo members
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
  
  df <- legco_api("schedule", query, 10000, verbose)
  
  if (!is.null(df)) {
    colnames(df) <- unify_colnames(colnames(df)) # in utils-misc.R
    
    # Combine duplicated entries of members who have served more than one term
    df$TermID[df$MemberID %in% df$MemberID[duplicated(df$MemberID)] & !duplicated(df$MemberID)] <- 
      sapply(1:sum(duplicated(df$MemberID)), function(x) { 
        # Combine multiple TermID of the same member into a single string
        list(c(df$TermID[df$MemberID %in% df$MemberID[duplicated(df$MemberID)] & !duplicated(df$MemberID)][x], 
              df$TermID[duplicated(df$MemberID)][x]))
      })
    df <- df[!duplicated(df$MemberID), ]
    rownames(df) <- 1:nrow(df)
    
    df
    
  }
}

#' @rdname member
#' @export
legco_member <- member
