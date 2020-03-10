#' Membership of LegCo Committees
#'
#' Fetch the members of LegCo committees.
#'
#' @param post The post of a member in the committee, or a vector of posts.
#'   `'President'`, `'Chairman'`, `'Deputy Chairman'`, `'Member'` and
#'   `'Convenor'` return members of the respective post. If `NULL`, returns
#'   members of all posts. Defaults to `NULL`.
#'
#' @inheritParams hansard
#' @inheritParams member
#' @inheritParams committee
#' @inheritParams term
#'
#' @examples 
#' \dontrun{
#' # Fetch members of the Subcommittee on Issues Relating to Bazaars
#' membership(committee_id = 2704)
#' }
#'
#' @export
#' 
membership <- function(member_id = NULL, committee_id = NULL, term_id = NULL, post = NULL,
                       n = 10000, extra_param = NULL, count = FALSE, verbose = TRUE) {
  query <- "Tmembership?$select=member_id,committee_id,post_eng,post_chi,post_start_date,post_end_date,term_id,Tcommittee,Tmember&$expand=Tcommittee,Tmember"
  
  filter_args <- {}
  
  if (!is.null(member_id)) {
    filter_args <- c(filter_args, generate_filter("member_id", member_id))
  }
  
  if (!is.null(committee_id)) {
    filter_args <- c(filter_args, generate_filter("committee_id", committee_id))
  }
  
  if (!is.null(term_id)) {
    filter_args <- c(filter_args, generate_filter("term_id", term_id))
  }
  
  if (!is.null(post)) {
    post <- capitalise(post)
    filter_args <- c(filter_args, generate_filter("post_eng", post))
  }
  
  if(!is.null(filter_args)) {
    query <- paste0(query, "&$filter=", paste(filter_args, collapse = " and "))
  }
  
  if (!is.null(extra_param)) {
    query <- paste0(query, extra_param)
  }
  
  df <- legco_api("schedule", query, n, count, verbose)
  
  if (!count) {
    colnames(df) <- unify_colnames(colnames(df)) # in utils-misc.R
    df <- df[, c(1:2, 10:11, 3:7, 22, 17, 19:20, 24, 16, 18, 21, 23)]
  }
  
  df
}

#' @rdname membership
#' @export
legco_membership <- membership
