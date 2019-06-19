## Function to generate one single filter statement from multiple matching values for the LegCo API
generate_filter <- function(var_name, var_values) {
  if(is.numeric(var_values)) {
    filter_args <- paste0(var_name, " eq ", var_values)
  } else {
    filter_args <- paste0(var_name, " eq '", var_values, "'")
  }
  filter_args <- paste0("(", paste(filter_args, collapse = " or "), ")")
  filter_args
}

## Function to unify format of column names (e.g. from 'id_name' to 'IdName')
unify_colnames <- function(col_names) {
  sapply(col_names, function(x) {
    x <- unlist(strsplit(x, "_"))
    x <- paste(paste0(toupper(substring(x, 1, 1)), substring(x, 2)), collapse = "")
  })
}
