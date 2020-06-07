#' Speakers at LegCo
#'
#' Fetch the basic information of speakers in LegCo council meetings, including
#' LegCo members, government officials and Secretariat staff.
#'
#' This function corresponds to the \emph{Speakers} data endpoint of the Hansard
#' Database.
#'
#' @param type the position of a speaker. \code{"all"} returns all speakers.
#'   \code{"PO"} returns public officers. \code{"LC"} returns key appointment
#'   holders and staff at LegCo, such as President, Chairman and clerk.
#'   \code{"MB"} returns LegCo members. Default to \code{"all"}.
#'
#' @inherit hansard-db
#'
#' @examples
#' \donttest{
#' # Fetch a list of all speakers in LegCo council meetings
#' x <- speakers()
#' # Fetch a list of all speakers who are LegCo members
#' x <- speakers(type = "MB")
#' # Look up the details of a speaker with the id 6
#' x <- speakers(speaker_id = 6)
#' }
#'
#' @export
#' 
speakers <- function(speaker_id = NULL, type = "all", extra_param = NULL,
                     count = FALSE, verbose = TRUE) {
  query <- "Speakers?"
  
  filter_args <- {}
  
  if (!is.null(speaker_id)) {
    filter_args <- c(filter_args, .generate_filter("SpeakerID", speaker_id))
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
  
  legco_api("hansard", query, 1000, count, verbose)
  }

#' @rdname speakers
#' @export
legco_speakers <- speakers
