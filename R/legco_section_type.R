#' Section Types of LegCo Meetings
#'
#' A data frame of possible types of sections in LegCo meetings. For use to look
#' up the meaning of a section code returned by the API.
#'
#' @format A dataframe with 51 rows and 3 variables: \itemize{ \item
#'   \code{SectionCode}: Unique code(s) for each section type \item
#'   \code{NameEng}: English name of the section \item \code{NameChi}: Chinese
#'   name of the section }
#'
#' @examples
#' # Look up the meaning of section code "mbm"
#' legco_section_type[legco_section_type$SectionCode == "mbm", ]
#' 
"legco_section_type"
