% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/voting_record.R
\name{voting_record}
\alias{voting_record}
\alias{legco_voting_record}
\title{Voting Record in LegCo Meetings}
\usage{
voting_record(
  committee = NULL,
  term_id = NULL,
  result = "all",
  vote = "all",
  name_ch = NULL,
  name_en = NULL,
  separate_mechanism = NULL,
  mover_type = "all",
  from = "1900-01-01 00:00:00",
  to = Sys.time(),
  n = 10000,
  extra_param = NULL,
  count = FALSE,
  verbose = TRUE
)

legco_voting_record(
  committee = NULL,
  term_id = NULL,
  result = "all",
  vote = "all",
  name_ch = NULL,
  name_en = NULL,
  separate_mechanism = NULL,
  mover_type = "all",
  from = "1900-01-01 00:00:00",
  to = Sys.time(),
  n = 10000,
  extra_param = NULL,
  count = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{committee}{the name of the committee or subcommittee. Defaults to
\code{NULL}.}

\item{term_id}{the id of a term, or a vector of ids. If \code{NULL}, returns
results of all terms. Defaults to \code{NULL}.}

\item{result}{the voting result. If \code{"passed"}, returns motions that
have been passed. If \code{"vetoed"}, returns motions that have been
vetoed.If \code{"all"}, returns all motions that has been voted in LegCo.
Defaults to \code{all}.}

\item{vote}{the vote cast. If \code{"yes"}, returns only members who cast
affirmative votes. If \code{"no"}, returns only members who cast negative
votes. If \code{"abstain"}, returns only members who abstained from voting.
If \code{"absent"}, returns only members who were absent. If
\code{"present"}, returns only members who were present and did not vote
(e.g. President). If \code{"all"}, returns all votes. Defaults to
\code{"all"}.}

\item{name_ch}{the name of a LegCo member in Traditional Chinese, or a vector
of names. If \code{NULL}, returns voting records of all members. Defaults
to \code{NULL}.}

\item{name_en}{the name of a LegCo member in English, or a vector of names.
If the member has an English name, the English name should go first
followed by the surname in capital letters, e.g. \code{"Peter CHAN"}. If
the member does not have an English name, the surname in capital letters
should go first followed by the translated first name with a hyphen
separating the different syllable, e.g. \code{"CHAN Tai-man"}. Check the
names of the members with \code{\link{member}()}. If \code{NULL}, returns
voting records of all members. Defaults to \code{NULL}.}

\item{separate_mechanism}{only fetch votes that were counted with the vote
separate mechanism, i.e. requiring majority in both geographical and
functional constituencies to pass. If \code{NULL}, returns all votes
regardless of the vote counting mechanism used. Defaults to \code{NULL}.}

\item{mover_type}{the type of motion being put on vote. If \code{"PO"},
returns votes on government motions only. If \code{"MB"}, returns votes on
members' motions only. If \code{"all"}, returns votes on all motions.
Defaults to \code{"all"}.}

\item{from}{only fetch results of meetings on or after this date and time.
Accepts character values in \code{"YYYY-MM-DD"} or \code{"YYYY-MM-DD
HH:MM:SS"} format, and objects of class \code{Date}, \code{POSIXt},
\code{POSIXct}, \code{POSIXlt} or anything else that can be coerced to a
date with \code{as.Date()}. Defaults to \code{"1900-01-01 00:00:00"}.}

\item{to}{only fetch results of meetings on or before this date and time.
Accepts character values in \code{"YYYY-MM-DD"} or \code{"YYYY-MM-DD
HH:MM:SS"} format, and objects of class \code{Date}, \code{POSIXt},
\code{POSIXct}, \code{POSIXlt} or anything else that can be coerced to a
date with \code{as.Date()}. Defaults to the current system time.}

\item{n}{the number of record to fetch. Defaults to \code{1000}.}

\item{extra_param}{additional query parameters defined in LegCo API. Must
begin with \code{"&"}.}

\item{count}{logical: Whether to return only the total count of records that
matches the parameter(s) instead of the result. Defaults to \code{FALSE}.}

\item{verbose}{logical: Whether to display progress messages when fetching
data? Defaults to \code{TRUE}.}
}
\description{
Fetch voting records of LegCo council, the Finance Committee and its
subcommittees and the House Committee meetings.
}
\details{
This function corresponds to the \emph{vVotingResult} data endpoint of the
Voting Result Database.
}
\examples{
\donttest{
# Fetch how members voted the motion on
# Abolishing the MPF Offsetting Mechanism on November 11, 2016
x <- voting_record(committee = "Council Meeting",
                   from = "2016-11-16 13:51:53",
                   to = "2016-11-16 13:51:53")
}

}
\seealso{
LegCo API documentation for the Voting Record database:
  \url{https://www.legco.gov.hk/odata/english/vrdb.html}
}
