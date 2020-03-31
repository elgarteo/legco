#' legco: Accessing the Hong Kong Legislative Council API
#'
#' Provides functions to fetch data from the Hong Kong Legislative Council API.
#'
#' @section Details: Each function of this package corresponds to the data
#'   endpoints of the API. It is therefore necessary to understand the structure
#'   of the API in order to extract the data needed. Please refer to the
#'   vignettes for more details.
#'
#'   This package supports five databases of the LegCo API: \emph{Bills},
#'   \emph{Hansard}, \emph{Meeting Attendance}, \emph{Meeting Schedule} and
#'   \emph{Voting Result}. It is essential to understand what data these
#'   databases store in order to utilise the API effectively. Please refer to
#'   the vignettes and the API documentations for more details (links in ‘See
#'   Also’).
#'
#' @section API Limits: The LegCo API does not have a specified rate limit, but
#'   in general the limit is approximately 1000 requests per IP per hour. When
#'   the rate limit is reached, the server will return an empty json.
#'
#'   LegCo's API server also has a node count limit of 100 nodes per request,
#'   which can be translated as 20 filtering conditions per request in most
#'   cases in meaningful term. This package automatically blocks requests that
#'   exceed the node count.
#'
#'   It is common for the connection to the LegCo API to experience SSL error
#'   from time to time, especially during repeated requests. This can usually be
#'   resolved simply by retrying. This package automatically retries the request
#'   once when an SSL error occurs.
#'
#'   Another common problem is that the LegCo API sometimes returns an empty
#'   json file when it is not supposed to. Again, this can usually be resolved
#'   by retrying. This package automatically retries the request once to make
#'   sure that an invalid search query or rate limit is not the cause of the
#'   problem.
#'
#' @section Functions: Generic function: \itemize{\item\code{\link{legco_api}}:
#'   Generic LegCo API}
#'
#'   Functions of the Bills database: \itemize{ \item \code{\link{all_bills}}:
#'   All Bills discussed in LegCo }
#'
#'   Functions of the Meeting Attendance Database: \itemize{ \item
#'   \code{\link{attendance}}: Attendance of members }
#'
#'   Functions of the Voting Result Database: \itemize{ \item
#'   \code{\link{voting_record}}: Voting record in LegCo meetings }
#' @inheritSection hansard-db Functions
#' @inheritSection meeting_schedule-db Functions
#'
#' @section Notes: In addition to the standard function names, each function in
#'   this package has a wrapper where the name is prefixed with \code{legco_}.
#'   For example, both \code{speakers()} and \code{legco_speakers()} will return
#'   the same result. This is because function names are taken from the data
#'   endpoints provided by the API on, which nonetheless are often not very
#'   informative and could clash with functions in other packages (e.g.
#'   \code{speakers()} is not a term unique to LegCo).
#'
#' @section Disclaimer: This package is not officially related to or endorsed by
#'   the Legislative Council of Hong Kong.
#'
#'   The Legislative Council of Hong Kong is the copyright owner of data
#'   retrieved from its open data API.
#'
#' @author Elgar Teo (\email{elgarteo@@connect.hku.hk})
#'
#' @seealso GitHub page: \url{https://github.com/elgarteo/legco/}
#'
#'   Online Vignettes: \url{https://elgarteo.github.io/legco/}
#'
#'   LegCo API Documentations \itemize{ \item Bills Database:
#'   \url{https://www.legco.gov.hk/odata/english/billsdb.html} \item Hansard
#'   Database: \url{https://www.legco.gov.hk/odata/english/hansard-db.html}
#'   \item Meeting Attendance Database:
#'   \url{https://www.legco.gov.hk/odata/english/attendance-db.html} \item
#'   Meeting Schedule Database:
#'   \url{https://www.legco.gov.hk/odata/english/schedule-db.html} \item Voting
#'   Result Database: \url{https://www.legco.gov.hk/odata/english/vrdb.html} }
#'
#' @docType package
#' @keywords internal
#' @importFrom httr accept_json content GET
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#'   
"_PACKAGE"
