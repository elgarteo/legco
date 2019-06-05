#' Hansards of LegCo
#' 
#' Fetch metadata and URLs of hansards of LegCo..
#' 
#' @param id The id of a hansard file. If `NULL`,
#' returns a list of all hansard files. Defaults to `NULL`.
#' 
#' @param lang The language of hansard. `'en'` returns the English version.
#' `'zh'` returns the Traditional Chinese version. Defaults to `'en'`.
#' 
#' @param from Only includes hansards of meetings on or after this date.
#' Accepts character values in `'YYYY-MM-DD'` format, and objects of
#' class `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or
#' anything else that can be coerced to a date with `as.Date()`.
#' Defaults to `'1900-01-01'`.
#' 
#' @param to Only includes hansards of meetings on or before this date.
#' Accepts character values in `'YYYY-MM-DD'` format, and objects of
#' class `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or
#' anything else that can be coerced to a date with `as.Date()`.
#' Defaults to the current system date.
#' 
#' @param floor Whether to fetch the floor version of the hansard files.
#' The floor version is the first presented version of hansard file in the
#' original language delivered by the speakers in LegCo. 
#' If `'TRUE'`, the language option is ignored. Defaults to `FALSE`.
#' 
#' @param n The number of hansard files to request. Defaults to `1000`.
#' 
#' @param extra_args Additional query string options defined in LegCo API.
#' Must start with `'&'`.
#' 
#' @param verbose Defaults to `TRUE`.
#' 
#' @export

hansard <- function(id = NULL, lang = "en", from = '1900-01-01', to = Sys.Date(), floor = FALSE, 
                    n = 1000, extra_args = NULL, verbose = TRUE) {
  baseurl <- paste0(hansard_base_url, "Hansard?$format=json&$inlinecount=allpages", 
                    "&$select=HansardID,MeetingDate,AgendaDate,isCEQandA,isCEQT,HansardFileURL")
  
  filter_args <- {}
  
  if (!is.null(id)) {
    filter_args <- c(filter_args, paste0("HansardID eq ", id))
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
  
  if (n < 1000) {
    paste0(baseurl, "&$top=", n)
  }
  
  if (!is.null(extra_args)) {
    baseurl <- paste0(baseurl, extra_args)
  }
  
  if (verbose) {
    message("Connecting to API...")
    message(paste0("Retrieving ", ifelse(n < 1000, n, 1000), " records..."))
  }
  
  baseurl <- paste0(baseurl, "&$filter=", paste(filter_args, collapse = " and "))
  
  baseurl <- utils::URLencode(baseurl)
  df <- jsonlite::fromJSON(baseurl, flatten = TRUE)
  
  total <- as.numeric(df$odata.count)
  
  if (nrow(df$value) == 0) {
    message("The request did not return any data. Please check your parameters.")
  } else if (n <= 1000 | total < 1000) {
    if (verbose) {
      message(paste0("Retrieved ", nrow(df$value), " records. ",
                     total, " records available in total."))
    }
    
    df$value
    
  } else {
    remaining <- ifelse(n > total, total - 1000, n - 1000)
    df <- rbind(df$value, fetch_remaining(df$odata.nextLink, remaining, verbose)) # in utils-loop.R
    
    if (verbose) {
      message(paste0("Retrieved ", nrow(df), " records. ",
                     total, " records available in total."))
    }
    
    df
    
  }
}

#' @rdname hansard
#' @export
legco_hansard <- hansard