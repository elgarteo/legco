#' Section Types of Legco Meetings
#' 
#' A data frame of possible types of sections in LegCo meetings.
#' For use to look up the meaning of a section code returned by the API.
#'
#' @format A dataframe with 35 rows and 3 variables:
#' \describe{
#'   \item{SectionCode}{Unique code(s) for each section type}
#'   \item{NameChi}{Chinese name of the section}
#'   \item{NameEng}{English name of the section}
#' }
"legco_section_type"