## Function to generate one single filter statement from multiple matching values for the LegCo API
generate_filter <- function(var_name, var_values) {
  if (is.numeric(var_values)) {
    # Check if vector is sequential
    if (sum(abs(diff(var_values))) == length(var_values) ) {
      filter_args <- paste0("(", var_name, " ge ", min(var_values), " and ",
                            var_name, " le ", max(var_values), ")")
    } else {
      filter_args <- paste0(var_name, " eq ", var_values)
    }
  } else {
    filter_args <- paste0(var_name, " eq '", var_values, "'")
  }
  filter_args <- paste0("(", paste(filter_args, collapse = " or "), ")")
  
  filter_args
}

## Function to unify format of column names (e.g. from 'id_name' to 'IdName')
unify_colnames <- function(col_names) {
  sapply(col_names, function(x) {
    x <- gsub("id$", "ID", x)
    x <- unlist(strsplit(x, "_"))
    x <- paste(paste0(toupper(substring(x, 1, 1)), substring(x, 2)), collapse = "")
  })
}

## Function to capitalise first letter of every word in a string except for prepositions
capitalise <- function(string) {
  string <- strsplit(string, " ")[[1]]
  string <- sapply(string, function(x)
    ifelse(tolower(x) %in% c("the", "of", "for", "to", "at", "on", "by"), 
           tolower(x), 
           paste0(toupper(substring(x, 1, 1)), substring(x, 2))))
  string <- paste(string, collapse = " ")
  string <- paste0(toupper(substring(string, 1, 1)), substring(string, 2))
  
  string
}
