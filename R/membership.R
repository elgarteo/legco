#' Membership of LegCo Committees
#'
#' Fetch the members of LegCo committees.
#'
#' @param id The id of a committee membership, or a vector of ids. If `NULL`, returns
#'   all committee membership. Defaults to `NULL`.
#'
#' @param member_id The code of a LegCo member, or a vector of ids. If `NULL`,
#'   returns all LegCo members. Defaults to `NULL`.
#'
#' @param committee_id The code of a LegCo committee, or a vector of ids. If
#'   `NULL`, returns records of all LegCo committees. Defaults to `NULL`.
#'
#' @param term_id The id of a term, or a vector of ids. If `NULL`, returns
#'   membership of committees of all terms.
#'
#' @param post The post of a member in the committee, or a vector of posts.
#'   `'President'`, `'Chairman'`, `'Deputy Chairman'`, `'Member'` and
#'   `'Convenor'` return members of the respective post. If `NULL`, returns
#'   members of all posts. Defaults to `NULL`.
#'
#' @param n The number of records to request. Defaults to `10000`.
#'
#' @param extra_param Additional query parameters defined in LegCo API. Must
#'   begin with `'&'`.
#'
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
membership <- function(id = NULL, member_id = NULL, committee_id = NULL, term_id = NULL,
                       post = NULL, n = 10000, extra_param = NULL, verbose = TRUE) {
  query <- "Tmembership?$select=membership_id,member_id,committee_id,post_eng,post_chi,post_start_date,post_end_date,term_id"
  
  filter_args <- {}
  
  if (!is.null(id)) {
    filter_args <- c(filter_args, generate_filter("membership_id", id))
  }
  
  if (!is.null(member_id)) {
    code <- toupper(code)
    filter_args <- c(filter_args, generate_filter("member_id", member_id))
  }
  
  if (!is.null(committee_id)) {
    filter_args <- c(filter_args, generate_filter("committee_id", committee_id))
  }
  
  if (!is.null(term_id)) {
    filter_args <- c(filter_args, generate_filter("term_id", term_id))
  }
  
  if (!is.null(post)) {
    # Capitalise the first letter of each word
    post <- sapply(post, function(x) {
      x <- tolower(x)
      x <- unlist(strsplit(x, " "))
      x <- paste0(toupper(substring(x, 1, 1)), substring(x, 2), collapse = " ")
    })
    filter_args <- c(filter_args, generate_filter("post_eng", post))
  }
  
  if(!is.null(filter_args)) {
    query <- paste0(query, "&$filter=", paste(filter_args, collapse = " and "))
  }
  
  if (!is.null(extra_param)) {
    query <- paste0(query, extra_param)
  }
  
  df <- legco_api("schedule", query, n, verbose)
  
  if (!is.null(df)) {
    colnames(df) <- unify_colnames(colnames(df)) # in utils-misc.R
  
    df
  }
}

#' @rdname membership
#' @export
legco_membership <- membership
