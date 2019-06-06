#' Generic LegCo API Function
#' 
#' A generic function to access LegCO APIs.
#' 
#' @param db The database you wish to access. `'hansard'` for the hansard database. 
#' `'attn'` for the attendance database. `'bill'` for the bills database.
#' Or the full path name for other database not listed here.
#' 
#' @param data The type of data you wish to receive. Attach your arguments as well if any.
#' 
#' @param n The number of entry to fetch. Default to `1000`.
#' 
#' @param verbose Defaults to `TRUE`.
#' 
#' @export
#' 
legco_api <- function(db, data, n, verbose) {
  db <- tolower(db)
  if (db == "hansard") {
    db <- "HansardDB"
  } else if (db == "attn") {
    db <- "AttendanceDB"
  } else if (db == "bill") {
    db <- "BillsDB"
  }
  
  baseurl <- paste0("https://app.legco.gov.hk/OpenData/", db, "/", data)
  
  if(stringr::str_detect(baseurl, '/.*\\?\\$')) { # Check if any query attached to URL
    baseurl <- paste0(baseurl, "&$format=json&$inlinecount=allpages")
  } else {
    baseurl <- paste0(baseurl, "?$format=json&$inlinecount=allpages")
  }
  
  if (n < 1000) {
    baseurl <- paste0(baseurl, "&$top=", n)
  }
  
  baseurl <- utils::URLencode(baseurl)
  
  if (verbose) {
    message("Connecting to API...")
    message(paste0("Retrieving records..."))
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
    df <- rbind(df$value, fetch_remaining(df$odata.nextLink, remaining, verbose)) # in utils-misc.R
    
    if (verbose) {
      message(paste0("Retrieved ", nrow(df), " records. ",
                     total, " records available in total."))
    }
    
    df
    
  }
}