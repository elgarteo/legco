% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/member_term.R
\name{member_term}
\alias{member_term}
\alias{legco_member_term}
\title{Terms of Members of LegCo}
\usage{
member_term(
  member_id = NULL,
  extra_param = NULL,
  count = FALSE,
  verbose = TRUE
)

legco_member_term(
  member_id = NULL,
  extra_param = NULL,
  count = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{member_id}{the id of a LegCo member, or a vector of ids. If
\code{NULL}, returns results of all LegCo members. Defaults to \code{NULL}.}

\item{extra_param}{additional query parameters defined in LegCo API. Must
begin with \code{"&"}.}

\item{count}{logical: Whether to return only the total count of records that
matches the parameter(s) instead of the result. Defaults to \code{FALSE}.}

\item{verbose}{logical: Whether to display progress messages when fetching
data? Defaults to \code{TRUE}.}
}
\description{
Fetch the serving terms in Term ID of LegCo members
}
\details{
This function corresponds to the \emph{Tmember_term} data endpoint of the
Meeting Schedule Database.
}
\section{Functions}{
 Functions of the Meeting Schedule Database: \itemize{
  \item \code{\link{term}}: LegCo terms \item \code{\link{session}}: LegCo
  sessions \item \code{\link{committee}}: LegCo committees \item
  \code{\link{membership}}: Membership of LegCo committees \item
  \code{\link{member}}: LegCo members \item \code{\link{member_term}}: Terms
  served by LegCo members \item \code{\link{meeting}}: Meetings of LegCo
  committees \item \code{\link{meeting_committee}}: Committees of LegCo
  meetings }
}

\examples{
\donttest{
# Fetches the term served by Hon Chan Kin-por and Kwong Chun-yu
x <- member_term(member_id = c(273, 924))
}

}
\seealso{
LegCo API documentation for the Meeting Schedule database:
  \url{https://www.legco.gov.hk/en/open-legco/open-data/meeting-schedule.html}
}
