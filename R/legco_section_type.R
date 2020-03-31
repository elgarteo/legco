#' Section Types of LegCo Meetings
#'
#' A data frame containing possible types of sections in LegCo meetings. For use
#' to look up the meaning of a section code returned by the API.
#'
#' This data frame is a formatted offline copy of the \emph{Sections} data
#' endpoint of the Hansard Database.
#'
#' @format A dataframe with 51 rows and 3 variables: \itemize{ \item
#'   \code{SectionCode}: Unique code(s) for each section type \item
#'   \code{NameEng}: English name of the section \item \code{NameChi}: Chinese
#'   name of the section }
#' 
#' @inherit hansard-db
#' 
#' @examples
#' # Look up the meaning of section code "mbm"
#' x <- legco_section_type[legco_section_type$SectionCode == "mbm", ]
#' 
"legco_section_type"
