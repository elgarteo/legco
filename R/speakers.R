#' Speakers at LegCo
#' 
#' Fetch the basic information of speakers in LegCo council meetings, including LegCo members,
#' government officials and Secretariat staff.
#' 
#' @param id The id of a speaker at the Legislative Council, or a vector of ids. If `NULL`,
#' returns a list of all speakers. Defaults to `NULL`.
#' 
#' @param type The position of a speaker. `'all'` returns all speakers.
#' `'PO'` returns public officers. `'LC'` returns key appointment holders and staff
#' at LegCo, such as President, Chariman and clerk.`'MB'` returns LegCo members.
#' Default to `'all'`.
#' 
#' @param extra_param Additional query parameters defined in LegCo API.
#' Must begin with `'&'`.
#' 
#' @param verbose Defaults to `TRUE`.
#' 
#' @export
#' 
speakers <- function(id = NULL, type = "all", extra_param = NULL, verbose = TRUE) {
  query <- "Speakers?"
  
  filter_args <- {}
  
  if (!is.null(id)) {
    filter_args <- c(filter_args, generate_filter("SpeakerID", id))
  }
  
  type <- toupper(type)
  if (type != "ALL") {
    filter_args <- c(filter_args, paste0("Type eq \'", type, "\'"))
  }
  
  if (!is.null(filter_args)) {
    query <- paste0(query, "$filter=", paste(filter_args, collapse = " and "))
  }
  
  if (!is.null(extra_param)) {
    query <- paste0(query, extra_param)
  }
  
  legco_api("hansard", query, n = 1000, verbose)
  }

#' @rdname speakers
#' @export
legco_speakers <- speakers
