## Function to generate one single filter statement from multiple matching values for the LegCo API
generate_filter <- function(var_name, var_values) {
  var_values <- unlist(var_values, use.names = FALSE)
  if (is.numeric(var_values)) {
    # Check if vector is sequential
    if (sum(abs(diff(var_values))) == (length(var_values) - 1) & length(var_values) > 1) {
      filter_args <- paste0(var_name, " ge ", min(var_values), " and ",
                            var_name, " le ", max(var_values))
    } else {
      filter_args <- paste0(var_name, " eq ", var_values)
    }
  } else {
    filter_args <- paste0(var_name, " eq '", var_values, "'")
  }
  filter_args <- paste0("(", paste(filter_args, collapse = " or "), ")")
  filter_args
}

## Function to unify format of column names (e.g. from 'member_id' to 'MemberID')
unify_colnames <- function(col_names) {
  sapply(col_names, function(x) {
    x <- gsub("^.*\\.", "", x)
    x <- unlist(strsplit(x, "_"), use.names = FALSE)
    x <- capitalise(x)
    x <- gsub(" ", "", x)
    x <- gsub("[Ii]{1}d$", "ID", x)
  })
}

## Function to capitalise first letter of every word in a string except for prepositions
capitalise <- function(string) {
  string <- unlist(strsplit(string, " "), use.names = FALSE)
  string <- sapply(string, function(x)
    ifelse(tolower(x) %in% c("the", "of", "for", "to", "at", "on", "by"), 
           tolower(x), 
           paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)))))
  string <- paste(string, collapse = " ")
  string <- paste0(toupper(substring(string, 1, 1)), substring(string, 2))
  string
}

## Function to calculate node count of query
node_count <- function(url) {
  url <- gsub(".*\\$filter=|\\&\\$.*", "", url)
  url <- unlist(strsplit(url, " and "), use.names = FALSE)
  url <- strsplit(url, " or ")
  n <- length(url) - 1 # 'and' count
  for (i in 1:length(url)) {
    n <- n + length(url[[i]]) - 1 # 'or' count
    n <- n + sum(ifelse(grepl("datetime", url[[i]]), 5, 4)) # individual condition count
  }
  n
}

## Function to convert time into .NET datetime string
posixlt2net <- function(string) {
  string <- as.character(string)
  if (!grepl("T", string)) {
    string <- format(as.POSIXlt(string), "%Y-%m-%dT%H:%M:%S")
  }
  string
}

## Function to convert .NET datetime string in POSIXlt compatiable in a dataframe
net2posixlt <- function(df) {
  time_col <- grep("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}", df[1, ])
  if (length(time_col)) {
    for (i in time_col) {
      df[, i] <- gsub("T", " ", df[, i])
    }
  }
  df
}

## Function to convert TermNo into TermID
convert_term_no <- function(term_no) {
  term_no - 1
}

## Function to convert TermID into TermNo
convert_term_id <- function(term_id) {
  term_id + 1
}
