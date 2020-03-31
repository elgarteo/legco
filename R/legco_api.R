#' Generic LegCo API Function
#'
#' A generic function to access LegCo APIs.
#'
#' @param db the database you wish to access. \code{"hansard"} for the hansard
#'   database. \code{"attn"} for the attendance database. \code{"bill"} for the
#'   bills database. \code{"schedule"} for the schedule database. Or the path
#'   name for databases not listed here (i.e. the string between the domain name
#'   and data endpoints, e.g. \code{"OpenData/HansardDB"}).
#'
#' @param query the query for retrieving data. Should include the data endpoint
#'   and parameters if any.
#'
#' @param n the number of record to fetch. Defaults to \code{1000}.
#'
#' @param count logical: Whether to return only the total count of records that
#'   matches the parameter(s) instead of the result. Defaults to \code{FALSE}.
#'
#' @param verbose logical: Whether to display progress messages when fetching
#'   data? Defaults to \code{TRUE}.
#'
#' @examples
#' # Fetch data from the "bills" endpoint of the hansard database
#' \donttest{
#' x <- legco_api("hansard", "Bills")
#' }
#'
#' @export
#' 
legco_api <- function(db, query, n = 1000, count = FALSE, verbose = TRUE) {
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
  if (grepl('/.*\\?\\$', baseurl)) { 
    baseurl <- paste0(baseurl, "&$format=json&$inlinecount=allpages")
  } else if (grepl('/.*\\?', baseurl)) {
    baseurl <- paste0(baseurl, "$format=json&$inlinecount=allpages")
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
  df <- access_api(baseurl, verbose)
  total <- as.numeric(df$odata.count)
  
  if (count) {
    if (verbose) {
      message("Retrieved total count of record(s).")
    }
    return(df$odata.count)
  }
  
  if (n <= maximum | total < maximum) {
    # All data retrieved
    if (verbose) {
      message("Retrieved ", nrow(df$value), " record(s). ",
              total, " record(s) available in total.")
    }
    return(net2posixlt(df$value))
  }
  
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
    Sys.sleep(2) # Prevent back-to-back request
    tmp <- access_api(nexturl, verbose)
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
  net2posixlt(df)
}

# Function to fetch data from API
access_api <- function(url, verbose, ssl = FALSE, empty = FALSE) {
  tryCatch({
    url <- utils::URLencode(iconv(url, to = "UTF-8", toRaw = FALSE))
    json <- httr::GET(url, httr::accept_json())
    json <- httr::content(json, as = "text", encoding = "UTF-8")
    df <- jsonlite::fromJSON(json, flatten = TRUE)
    if (df$odata.count == 0) {
      # If no data retrieved, retry again to make sure it's not a connection problem
      if (empty) stop("The request did not return any data: ",
                      "Invalid search parameters or rate limit exceeded.")
      if (verbose) {
        message("The request did not return any data.")
        message("Possible common connection problem resolvable by retrying.")
        message("Retrying...")
      }
      Sys.sleep(2)
      return(access_api(url, verbose, empty = TRUE))
    }
    df
  }, error = function(cnd) {
    # Retry if error is a common LegCo API connection problem
    if (grepl("SSL_ERROR_SYSCALL", cnd[[1]]) & !ssl) {
      if (verbose) {
        message("Encountered ", cnd)
        message("Possible common connection problem resolvable by retrying.")
        message("Retrying...")
      }
      Sys.sleep(2)
      return(access_api(url, verbose, ssl = TRUE))
    } 
    stop(cnd)
  })
}
