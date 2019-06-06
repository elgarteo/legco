#' Speakers at LegCo
#' 
#' Fetch basic details of the speakers at LegCo, including LegCo members,
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
#' @param extra_args Additional query string options defined in LegCo API.
#' Start with `'&'` then followed by the option.
#' 
#' @param verbose Defaults to `TRUE`.
#' 
#' @export

speakers <- function(id = NULL, type = "all", extra_args = NULL, verbose = TRUE) {
  baseurl <- paste0(hansard_base_url, "Speakers?$format=json")
  
  filter_args <- {}
  
  if (!is.null(id)) {
    id <- paste("SpeakerID eq ", id)
    id <- paste(id, collapse = " or ")
    filter_args <- c(filter_args, id)
  }
  
  type <- toupper(type)
  if (type != "ALL") {
    filter_args <- c(filter_args, paste0("Type eq \'", type, "\'"))
  }
  
  if (!is.null(id)) {
    baseurl <- paste0(baseurl, "&$filter=SpeakerID eq ", id)
  }
  
  if (type != "all") {
    baseurl <- paste0(baseurl, "&$filter=Type eq \'", type, "\'")
  }
  
  if (!is.null(extra_args)) {
    baseurl <- paste0(baseurl, extra_args)
  }
  
  baseurl <- paste0(baseurl, "&$filter=", paste(filter_args, collapse = " and "))
  
  if (verbose) {
    message("Connecting to API...")
  }
  
  baseurl <- utils::URLencode(baseurl)
  df <- jsonlite::fromJSON(baseurl, flatten = TRUE)
  
  if (nrow(as.data.frame(df$value)) == 0) {
    message("The request did not return any data. Please check your parameters.")
  } else {
    df$value
  }
}

#' @rdname speakers
#' @export
legco_speakers <- speakers
