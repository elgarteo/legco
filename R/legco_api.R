#' Generic LegCo API Function
#'
#' A generic function to access LegCo APIs.
#'
#' @param db The database you wish to access. `'hansard'` for the hansard
#'   database. `'attn'` for the attendance database. `'bill'` for the bills
#'   database. Or the full path name for other database not listed here.
#'
#' @param query The query for retrieving data. Should include the data endpoint
#'   and parameters if any.
#'
#' @param n The number of entry to fetch. Default to `1000`.
#'
#' @param verbose Defaults to `TRUE`.
#' 
#' @export
#' 
legco_api <- function(db, query, n, verbose) {
  db <- tolower(db)
  if (db == "hansard") {
    db <- "HansardDB"
  } else if (db == "attn") {
    db <- "AttendanceDB"
  } else if (db == "bill") {
    db <- "BillsDB"
  }
  
  baseurl <- paste0("https://app.legco.gov.hk/OpenData/", db, "/", query)
  
  # Check if any parameter attached to query
  if(stringr::str_detect(baseurl, '/.*\\?\\$')) { 
    baseurl <- paste0(baseurl, "&$format=json&$inlinecount=allpages")
  } else {
    baseurl <- paste0(baseurl, "?$format=json&$inlinecount=allpages")
  }
  
  if (n < 1000) {
    baseurl <- paste0(baseurl, "&$top=", n)
  }
  
  baseurl <- utils::URLencode(baseurl)
  
  if (verbose) {
    message("Retrieving records...")
  }
  
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
    nexturl <- df$odata.nextLink
    df <- df$value
    
    if (verbose) {
      message(remaining, " record(s) remaining.")
    }
    
    for (i in 1:ceiling(remaining / 1000)) {
      if (remaining < 1000) {
        nexturl <- paste0(nexturl, "&$top=", remaining)
      }
      
      if (verbose) {
        message("Retrieving ", ifelse(remaining < 1000, remaining, 1000), " records...")
      }
      
      nexturl <- utils::URLencode(nexturl)
      tmp <- jsonlite::fromJSON(nexturl, flatten = TRUE)
      
      df <- rbind(df, tmp$value)
      
      if (remaining > 1000) {
        remaining <- remaining - 1000
        if (verbose) {
          message(remaining, " records remaining.")
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
