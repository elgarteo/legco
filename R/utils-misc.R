## Function to generate one single filter statement from multiple matching values for the LegCo API
generate_filter <- function(var_name, var_values) {
  filter_args <- paste0(var_name, " eq ", var_values)
  filter_args <- paste(filter_args, collapse = " or ")
  filter_args
}
