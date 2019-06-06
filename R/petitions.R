#' Petitions in LegCo
#' 
#' Fetch the petitions presented in LegCo.
#' 
#' @param rundown_id The id of a rundown, or a vector of ids.
#' Defaults to `NULL`.
#' 
#' @param hansard_id The id of a hansard file, or a vector of ids. If `NULL`,
#' returns all subjects from all hansard files. Defaults to `NULL`.
#' 
#' @param lang The language. `'en'` returns the English version.
#' `'zh'` returns the Traditional Chinese version. Defaults to `'en'`.
#' 
#' @param from Only fetch results from hansards of meetings on or after this date.
#' Accepts character values in `'YYYY-MM-DD'` format, and objects of
#' class `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or
#' anything else that can be coerced to a date with `as.Date()`.
#' Defaults to `'1900-01-01'`.
#' 
#' @param to Only fetch results from hansards of meetings on or before this date.
#' Accepts character values in `'YYYY-MM-DD'` format, and objects of
#' class `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or
#' anything else that can be coerced to a date with `as.Date()`.
#' Defaults to the current system date.
#' 
#' @param floor Whether to fetch results from the floor version of the hansard files.
#' The floor version is the first presented version of hansard file in the
#' original language delivered by the speakers in LegCo. 
#' If `'TRUE'`, the language option is ignored. Defaults to `FALSE`.
#' 
#' @param n The number of subjects to request. Defaults to `1000`.
#' 
#' @param extra_args Additional query string options defined in LegCo API.
#' Start with `'&'` then followed by the option.
#' 
#' @param verbose Defaults to `TRUE`.
#' 
#' @export
#' 
petitions <- function(rundown_id = NULL, hansard_id = NULL, section_code = NULL,
                     lang = "en", from = '1900-01-01', to = Sys.Date(),
                     floor = FALSE, n = 1000, extra_args = NULL, verbose = TRUE) {
  query <- "Petitions?$format=json&$inlinecount=allpages"
  
  filter_args <- {}
  
  if (!is.null(rundown_id)) {
    filter_args <- c(filter_args, generate_filter("RundownID", rundown_id))
  }

  if (!is.null(hansard_id)) {
    filter_args <- c(filter_args, generate_filter("HansardID", hansard_id))
  }
  
  if (!is.null(section_code)) {
    filter_args <- c(filter_args, generate_filter("SectionCode", section_code))
  }
  
  lang <- tolower(lang)
  if (floor) {
    filter_args <- c(filter_args, paste0("HansardType eq 'Floor'"))
  } else if (lang == "en") {
    filter_args <- c(filter_args, paste0("HansardType eq 'English'"))
  } else if (lang == "zh") {
    filter_args <- c(filter_args, paste0("HansardType eq 'Chinese'"))
  }
  
  from <- as.Date(from)
  to <- as.Date(to)
  filter_args <- c(filter_args, paste0("MeetingDate gt datetime\'", from, 
                                       "\' and MeetingDate lt datetime\'", to, "\'"))
  
  query <- paste0(query, "&$filter=", paste(filter_args, collapse = " and "))
  
  if (!is.null(extra_args)) {
    baseurl <- paste0(baseurl, extra_args)
  }
  
  legco_api("hansard", query, n, verbose)
  
}

#' @rdname petitions
#' @export
legco_petitions <- petitions
