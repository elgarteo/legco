## Retrieves data in multiple requests when requested size > 1000
fetch_remaining <- function(next_url, remaining, verbose) {
  if (verbose) {
    message(remaining, " records remaining.")
  }
  
  df <- data.frame()
  
  for (i in 1:ceiling(remaining/1000)) {
    if (remaining < 1000) {
      next_url <- paste0(next_url, "&$top=", remaining)
    }
    
    if (verbose) {
      message(paste0("Retrieving ", ifelse(remaining < 1000, remaining, 1000), " records..."))
    }
    
    next_url <- utils::URLencode(next_url)
    tmp <- jsonlite::fromJSON(next_url, flatten = TRUE)
    
    df <- rbind(df, tmp$value)
    
    if (remaining >= 1000) {
      remaining <- remaining - 1000
      if (verbose) {
        message(remaining, " records remaining.")
      }
      next_url <- tmp$odata.nextLink
    }
  }
  
  df
  
}