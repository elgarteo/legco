#' Petitions in LegCo
#'
#' Fetch petitions presented in LegCo council meetings.
#'
#' This function corresponds to the \emph{Petitions} data endpoint of the
#' Hansard Database.
#'
#' @inherit hansard-db
#' 
#' @examples
#' \donttest{
#' # Fetch petition laid during the Council meeting on January 19, 2018
#' x <- petitions(from = "2018-01-10")
#' }
#'
#' @export
#' 
petitions <- function(rundown_id = NULL, hansard_id = NULL, lang = "en",
                      from = '1900-01-01', to = Sys.Date(), floor = FALSE,
                      n = 1000, extra_param = NULL, count = FALSE, verbose = TRUE) {
  query <- "Petitions?$select=MeetingDate,Subject,Speakers,RundownID,HansardID,HansardFileURL"
  
  filter_args <- {}
  
  if (!is.null(rundown_id)) {
    filter_args <- c(filter_args, generate_filter("RundownID", rundown_id))
  }

  if (!is.null(hansard_id)) {
    filter_args <- c(filter_args, generate_filter("HansardID", hansard_id))
  }
  
  if (is.null(hansard_id) & is.null(rundown_id)) {
    lang <- tolower(lang)
    if (floor) {
      filter_args <- c(filter_args, "HansardType eq 'Floor'")
    } else if (lang == "en") {
      filter_args <- c(filter_args, "HansardType eq 'English'")
    } else if (lang == "zh") {
      filter_args <- c(filter_args, "HansardType eq 'Chinese'")
    }
  }
  
  from <- as.Date(from)
  to <- as.Date(to)
  filter_args <- c(filter_args, paste0("MeetingDate ge datetime\'", from, 
                                       "\' and MeetingDate le datetime\'", to, "\'"))
  
  query <- paste0(query, "&$filter=", paste(filter_args, collapse = " and "))
  
  if (!is.null(extra_param)) {
    query <- paste0(query, extra_param)
  }
  
  df <- legco_api("hansard", query, n, count, verbose)
  
  if (!count) {
    # Create vector if more than one petition sponsor
    df$Speakers <- lapply(df$Speakers, function(x) unlist(strsplit(x, ", ")))
  }
  
  df
}

#' @rdname petitions
#' @export
legco_petitions <- petitions
