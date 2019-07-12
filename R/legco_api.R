#' Generic LegCo API Function
#'
#' A generic function to access LegCo APIs.
#'
#' @param db The database you wish to access. `'hansard'` for the hansard
#'   database. `'attn'` for the attendance database. `'bill'` for the bills
#'   database. `'schedule'` for the schedule database. Or the path name for
#'   databases not listed here (i.e. the string between the domain name and data
#'   endpoints, e.g. `'OpenData/HansardDB'`).
#'
#' @param query The query for retrieving data. Should include the data endpoint
#'   and parameters if any.
#'
#' @param n The number of record to fetch. Defaults to `1000`.
#'
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
legco_api <- function(db, query, n = 1000, verbose = TRUE) {
  db <- tolower(db)
  if (db == "hansard") {
    db <- "OpenData/HansardDB"
    maximum <- 1000
  } else if (db == "attn") {
    db <- "OpenData/AttendanceDB"
    maximum <- 1000
  } else if (db == "bill") {
    db <- "BillsDB/odata"
    maximum <- 10000
  } else if (db == "schedule") {
    db <- "ScheduleDB/odata"
    maximum <- 10000
  } else {
    maximum <- 10000
  }
  
  baseurl <- paste0("https://app.legco.gov.hk/", db, "/", query)
  
  # Check if any parameter already attached to query
  if(stringr::str_detect(baseurl, '/.*\\?\\$')) { 
    baseurl <- paste0(baseurl, "&$format=json&$inlinecount=allpages")
  } else {
    baseurl <- paste0(baseurl, "?$format=json&$inlinecount=allpages")
  }
  
  if (n < maximum) {
    baseurl <- paste0(baseurl, "&$top=", n)
  }
  
  if (node_count(baseurl) > 100) { # in utils-misc.R
    stop("Parameters too long. Please shorten your parameters and break down into multiple requests.")
  }
  
  if (verbose) {
    message("Retrieving records...")
  }
  
  baseurl <- utils::URLencode(baseurl)
  df <- jsonlite::fromJSON(baseurl, flatten = TRUE)
  
  total <- as.numeric(df$odata.count)
  
  if (df$odata.count == 0) {
    # No data retrieved
    stop("The request did not return any data. Please check your parameters.")
  }
  
  if (n <= maximum | total < maximum) {
    # All data retrieved
    if (verbose) {
      message("Retrieved ", nrow(df$value), " record(s). ",
              total, " record(s) available in total.")
    }
    
    df$value
  } else {
    # Only partial data retrieved
    remaining <- ifelse(n > total, total - maximum, n - maximum)
    nexturl <- df$odata.nextLink
    df <- df$value
    
    if (verbose) {
      message("Retrieved ", nrow(df), " records. ",
              remaining, " record(s) remaining.")
    }
    
    for (i in 1:ceiling(remaining / maximum)) {
      if (remaining < maximum) {
        nexturl <- paste0(nexturl, "&$top=", remaining)
      }
      
      if (verbose) {
        message("Retrieving ", ifelse(remaining < maximum, remaining, maximum), " records...")
      }
      
      Sys.sleep(2) # Enhance stability by preventing back-to-back request
      nexturl <- utils::URLencode(nexturl)
      tmp <- jsonlite::fromJSON(nexturl, flatten = TRUE)
      
      df <- rbind(df, tmp$value)
      
      if (remaining > maximum) {
        remaining <- remaining - maximum
        if (verbose) {
          message("Retrieved ", nrow(df), " records. ",
                  remaining, " record(s) remaining.")
        }
        nexturl <- tmp$odata.nextLink
      }
    }
    
    if (verbose) {
      message("Retrieved ", nrow(df), " records. ",
              total, " records available in total.")
    }
    
    df
  }
}
