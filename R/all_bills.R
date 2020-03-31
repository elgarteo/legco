#' All Bills in LegCo
#'
#' Fetch the information of bills presented in LegCo since 1906.
#'
#' This function corresponds to the \emph{Vbills} data endpoint of the Bills
#' Database.
#'
#' @param id the id of a bill, or a vector of ids. Defaults to \code{NULL}.
#'
#' @param ordinance the ordinance associated with the bill. Takes the full
#'   English name of the ordinance, e.g. \code{"Buildings Ordinanace"}. Defaults
#'   to \code{NULL}.
#'
#' @param title the title of the bill. Takes the full English name of the bill,
#'   e.g. \code{"National Anthem Bill"}. Defaults to \code{NULL}.
#'
#' @param proposer the proposer of the bill. Takes the full English title of the
#'   proposer, e.g. \code{"Secretary for Security"}. Defaults to \code{NULL}.
#'
#' @param gazette_from only fetch bills gazetted on or after this date. Accepts
#'   \code{"all"}, \code{NULL} or character values in \code{"YYYY-MM-DD"}
#'   format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct},
#'   \code{POSIXlt} or anything else that can be coerced to a date with
#'   \code{as.Date()}. If \code{"all"}, returns all bills that have and have not
#'   been gazetted before being tabled in LegCo and overrides the
#'   \code{gazette_to} parameter. If \code{NULL}, returns bills that have not
#'   been gazetted before being tabled in LegCo (mainly older bills as newer
#'   ones required to be gazetted before tabling as required by the Rules of
#'   Procedure) and overrides the \code{gazette_to} parameter. Defaults to
#'   \code{"all"}.
#'
#' @param gazette_to only fetch bills gazetted on or before this date. Accepts
#'   \code{"all"}, \code{NULL} or character values in \code{"YYYY-MM-DD"}
#'   format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct},
#'   \code{POSIXlt} or anything else that can be coerced to a date with
#'   \code{as.Date()}. If \code{"all"}, returns all bills that have and have not
#'   been gazetted before being tabled in LegCo and overrides the
#'   \code{gazette_from} parameter. If \code{NULL}, returns bills that have not
#'   been gazetted before being tabled in LegCo (mainly older bills as newer
#'   ones required to be gazetted before tabling as required by the Rules of
#'   Procedure) and overrides the \code{gazette_from} parameter. Defaults to
#'   \code{"all"}.
#'
#' @param first_from only fetch bills that have gone through first reading on or
#'   after this date. Accepts \code{"all"}, \code{NULL} or character values in
#'   \code{"YYYY-MM-DD"} format, and objects of class \code{Date},
#'   \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else that can be
#'   coerced to a date with \code{as.Date()}. If \code{"all"}, returns all bills
#'   that have and have not gone through first reading and overrides the
#'   \code{first_to} parameter. If \code{NULL}, returns bills that have not gone
#'   through first reading and overrides the \code{first_to} parameter. Defaults
#'   to \code{"all"}.
#'
#' @param first_to only fetch bills that have gone through first reading on or
#'   before this date. Accepts \code{"all"}, \code{NULL} or character values in
#'   \code{"YYYY-MM-DD"} format, and objects of class \code{Date},
#'   \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else that can be
#'   coerced to a date with \code{as.Date()}. If \code{"all"}, returns all bills
#'   that have and have not gone through first reading and overrides the
#'   \code{first_from} parameter. If \code{NULL}, returns bills that have not
#'   gone through first reading and overrides the \code{first_from} parameter.
#'   Defaults to \code{"all"}.
#'
#' @param second_from only fetch bills that have gone through second reading on
#'   or after this date. Accepts \code{"all"}, \code{NULL} or character values
#'   in \code{"YYYY-MM-DD"} format, and objects of class \code{Date},
#'   \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else that can be
#'   coerced to a date with \code{as.Date()}. If \code{"all"}, returns all bills
#'   that have and have not gone through second reading and overrides the
#'   \code{second_to} parameter. If \code{NULL}, returns bills that have not
#'   gone through second reading and overrides the \code{second_to} parameter.
#'   Defaults to \code{"all"}.
#'
#' @param second_to only fetch bills that have gone through second reading on or
#'   before this date. Accepts \code{"all"}, \code{NULL} or character values in
#'   \code{"YYYY-MM-DD"} format, and objects of class \code{Date},
#'   \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else that can be
#'   coerced to a date with \code{as.Date()}. If \code{"all"}, returns all bills
#'   that have and have not gone through second reading and overrides the
#'   \code{second_from} parameter. If \code{NULL}, returns bills that have not
#'   gone through second reading and overrides the \code{second_from} parameter.
#'   Defaults to \code{"all"}.
#'
#' @param third_from only fetch bills that have gone through third reading on or
#'   after this date. Accepts \code{"all"}, \code{NULL} or character values in
#'   \code{"YYYY-MM-DD"} format, and objects of class \code{Date},
#'   \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else that can be
#'   coerced to a date with \code{as.Date()}. If \code{"all"}, returns all bills
#'   that have and have not gone through third reading and overrides the
#'   \code{third_to} parameter. If \code{NULL}, returns bills that have not gone
#'   through third reading and overrides the \code{third_to} parameter. Defaults
#'   to \code{"all"}.
#'
#' @param third_to only fetch bills that have gone through third reading on or
#'   before this date. Accepts \code{"all"}, \code{NULL} or character values in
#'   \code{"YYYY-MM-DD"} format, and objects of class \code{Date},
#'   \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else that can be
#'   coerced to a date with \code{as.Date()}. If \code{"all"}, returns all bills
#'   that have and have not gone through third reading and overrides the
#'   \code{third_from} parameter. If \code{NULL}, returns bills that have not
#'   gone through third reading and overrides the \code{third_from} parameter.
#'   Defaults to \code{"all"}.
#'
#' @inheritParams hansard-db
#'
#' @seealso LegCo API documentation for the Meeting Schedule database:
#'   \url{https://www.legco.gov.hk/odata/english/billsdb.html}
#'
#' @examples
#' \donttest{
#' # Fetch bills that passed third reading on February 20, 2019
#' x <- all_bills(third_from = "2019-02-20")
#' }
#'
#' @export
#' 
all_bills <- function(id = NULL, ordinance = NULL, title = NULL, proposer = NULL, 
                      gazette_from = 'all', gazette_to = 'all',
                      first_from = 'all', first_to = 'all', second_from = 'all',
                      second_to = 'all', third_from = 'all', third_to = 'all', 
                      n = 10000, extra_param = NULL, count = FALSE, verbose = TRUE) {
  query <- "Vbills?"
  
  filter_args <- {}
  
  if (!is.null(id)) {
    filter_args <- c(filter_args, generate_filter("internal_key", id))
  }

  if (!is.null(ordinance)) {
    ordinance <- capitalise(ordinance)
    filter_args <- c(filter_args, paste0("ordinance_title_eng eq '", ordinance, "'"))
  }
  
  if (!is.null(title)) {
    title <- capitalise(title)
    filter_args <- c(filter_args, paste0("bill_title_eng eq '", title, "'"))
  }
  
  if (!is.null(proposer)) {
    proposer <- capitalise(proposer)
    filter_args <- c(filter_args, paste0("proposed_by_eng eq '", proposer, "'"))
  }
  
  if (is.null(gazette_from) | is.null(gazette_to)) {
    filter_args <- c(filter_args, "bill_gazette_date eq null")
  } else if (gazette_from != "all" & gazette_to != "all") {
    gazette_from <- as.Date(gazette_from)
    gazette_to <- as.Date(gazette_to)
    filter_args <- c(filter_args, paste0("bill_gazette_date ge datetime\'", gazette_from, 
                                         "\' and bill_gazette_date le datetime\'", gazette_to, "\'"))
  } else if (gazette_from != "all") {
    gazette_from <- as.Date(gazette_from)
    filter_args <- c(filter_args, paste0("bill_gazette_date ge datetime\'", gazette_from, "\'"))
  } else if (gazette_to != "all") {
    gazette_to <- as.Date(gazette_to)
    filter_args <- c(filter_args, paste0("bill_gazette_date le datetime\'", gazette_to, "\'"))
  }

  if (is.null(first_from) | is.null(first_to)) {
    filter_args <- c(filter_args, "first_reading_date eq null")
  } else if (first_from != "all" & first_to != "all") {
    first_from <- as.Date(first_from)
    first_to <- as.Date(first_to)
    filter_args <- c(filter_args, paste0("first_reading_date ge datetime\'", first_from, 
                                         "\' and first_reading_date le datetime\'", first_to, "\'"))
  } else if (first_from != "all") {
    first_from <- as.Date(first_from)
    filter_args <- c(filter_args, paste0("first_reading_date ge datetime\'", first_from, "\'"))
  } else if (first_to != "all") {
    first_to <- as.Date(first_to)
    filter_args <- c(filter_args, paste0("first_reading_date le datetime\'", first_to, "\'"))
  }
  
  if (is.null(second_from) | is.null(second_to)) {
    filter_args <- c(filter_args, "second_reading_date eq null")
  } else if (second_from != "all" & second_to != "all") {
    second_from <- as.Date(second_from)
    second_to <- as.Date(second_to)
    filter_args <- c(filter_args, paste0("second_reading_date ge datetime\'", second_from, 
                                         "\' and second_reading_date le datetime\'", second_to, "\'"))
  } else if (second_from != "all") {
    second_from <- as.Date(second_from)
    filter_args <- c(filter_args, paste0("second_reading_date ge datetime\'", second_from, "\'"))
  } else if (second_to != "all") {
    second_to <- as.Date(second_to)
    filter_args <- c(filter_args, paste0("second_reading_date le datetime\'", second_to, "\'"))
  }
  
  if (is.null(third_from) | is.null(third_to)) {
    filter_args <- c(filter_args, "third_reading_date eq null")
  } else if (third_from != "all" & third_to != "all") {
    third_from <- as.Date(third_from)
    third_to <- as.Date(third_to)
    filter_args <- c(filter_args, paste0("third_reading_date ge datetime\'", third_from, 
                                         "\' and third_reading_date le datetime\'", third_to, "\'"))
  } else if (third_from != "all") {
    third_from <- as.Date(third_from)
    filter_args <- c(filter_args, paste0("third_reading_date ge datetime\'", third_from, "\'"))
  } else if (third_to != "all") {
    third_to <- as.Date(third_to)
    filter_args <- c(filter_args, paste0("third_reading_date le datetime\'", third_to, "\'"))
  }
  
  if (!is.null(filter_args)) {
    query <- paste0(query, "$filter=", paste(filter_args, collapse = " and ")) 
  }
  
  if (!is.null(extra_param)) {
    query <- paste0(query, extra_param)
  }
  
  df <- legco_api("bill", query, n, count, verbose)
  
  if (!count) {
    colnames(df) <- unify_colnames(colnames(df)) # in utils-misc.R
  }
  
  df
}

#' @rdname all_bills
#' @export
legco_all_bills <- all_bills
