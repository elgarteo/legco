#' Terms of Members of LegCo
#'
#' Fetch the serving terms in Term ID of LegCo members
#'
#' @inheritParams hansard
#' @inheritParams member
#' 
#' @examples 
#' \dontrun{
#' #Fetches the term served by Hon Chan Kin-por and Kwong Chun-yu
#' member_term(id = c(273, 924))
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
  
    # Combine duplicated entries of members who have served more than one term
    df$TermID[df$MemberID %in% df$MemberID[duplicated(df$MemberID)] & !duplicated(df$MemberID)] <- 
      lapply(1:sum(duplicated(df$MemberID)), function(x) { 
        # Combine multiple TermID of the same member into a single string
        c(df$TermID[df$MemberID %in% df$MemberID[duplicated(df$MemberID)] & !duplicated(df$MemberID)][x], 
          df$TermID[duplicated(df$MemberID)][x])
      })
    df <- df[!duplicated(df$MemberID), ]
    rownames(df) <- 1:nrow(df)
  }
  
  df
}

#' @rdname member_term
#' @export
legco_member_term <- member_term
