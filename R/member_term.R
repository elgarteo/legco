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
  
  df <- legco_api("schedule", query, 1000, verbose)
  
  if (!is.null(df)) {
    colnames(df) <- unify_colnames(colnames(df)) # in utils-misc.R
    
    # Combine duplicated entries of members who have served more than one term
    df$TermId[df$MemberId %in% df$MemberId[duplicated(df$MemberId)] & !duplicated(df$MemberId)] <- 
      sapply(1:sum(duplicated(df$MemberId)), function(x) { 
        # Combine multiple TermID of the same member into a single string
        paste(df$TermId[df$MemberId %in% df$MemberId[duplicated(df$MemberId)] & !duplicated(df$MemberId)][x], 
              df$TermId[duplicated(df$MemberId)][x])
      })
    df <- df[!duplicated(df$MemberId), ]
    rownames(df) <- c(1:nrow(df))
    # Convert string containing multiple TermIDs into vectors
    df$TermId <- sapply(df$TermId, function(x) strsplit(x, " "))
    df$TermId <- sapply(df$TermId, function(x) unname(x))
    
    df
  }
}

#' @rdname member
#' @export
legco_member <- member
