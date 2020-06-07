#' Hansard Database
#'
#' This database contains information on matters discussed in Council meeting
#' since the fifth term of LegCo. Hinting by its name, the database is built
#' upon the PDF hansard files. Relying on the bookmarks and section codes of the
#' hansard files, the data endpoints retrieve data from the files directly.
#'
#' @param hansard_id the id of a hansard file, or a vector of ids. If
#'   \code{NULL}, returns results of all hansard files. Defaults to \code{NULL}.
#'   
#' @param speaker_id the id of a speaker at the Legislative Council, or a vector
#'   of ids. If \code{NULL}, returns all speakers. Defaults to \code{NULL}.
#'   
#' @param rundown_id the id of a rundown, or a vector of ids. If \code{NULL},
#'   returns results of all rundowns. Defaults to \code{NULL}.
#'
#' @param bookmark_name the bookmark name of the rundown, or a vector of names.
#'   Defaults to \code{NULL}.
#'   
#' @param section_code the section code, or a vector of codes. If \code{NULL},
#'   returns results of sections. Defaults to \code{NULL}.
#'
#' @param lang the language of hansard files to search from. \code{"en"} returns
#'   the English version. \code{"zh"} returns the Traditional Chinese version.
#'   Defaults to \code{"en"}.
#'
#' @param from only fetch results of meetings on or after this date. Accepts
#'   character values in \code{"YYYY-MM-DD"} format, and objects of class
#'   \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else
#'   that can be coerced to a date with \code{as.Date()}. Defaults to
#'   \code{"1900-01-01"}.
#'
#' @param to only fetch results of meetings on or before this date. Accepts
#'   character values in \code{"YYYY-MM-DD"} format, and objects of class
#'   \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else
#'   that can be coerced to a date with \code{as.Date()}. Defaults to the
#'   current system date.
#'
#' @param floor logical: whether to fetch results from the floor version of the
#'   hansard files? The floor version is the first presented version of hansard
#'   file in the original language delivered by the speakers in LegCo. If
#'   \code{TRUE}, the language option is ignored. Defaults to \code{FALSE}.
#'   
#' @param result the voting result. \code{"passed"} returns motions that have
#'   been passed. \code{"vetoed"} returns motions that have been vetoed.
#'   \code{"all"} returns all motions that has been voted in LegCo. Defaults to
#'   \code{all}.
#'
#' @param extra_param additional query parameters defined in LegCo API. Must
#'   begin with \code{"&"}.
#'
#' @param n the number of record to fetch. Defaults to \code{1000}.
#'
#' @param count logical: Whether to return only the total count of records that
#'   matches the parameter(s) instead of the result. Defaults to \code{FALSE}.
#'
#' @param verbose logical: Whether to display progress messages when fetching
#'   data? Defaults to \code{TRUE}.
#'
#' @section Functions: Functions of the Hansard database: \itemize{ \item
#'   \code{\link{hansard}}: Hansard files \item
#'   \code{\link{legco_section_type}}: Section code \item
#'   \code{\link{subjects}}: Subjects \code{\link{speakers}}: Speakers in the
#'   council, including members, government officials and secretariat staff
#'   \item \code{\link{rundown}}: Rundown (Paragraphs in hansard) \item
#'   \code{\link{questions}}: Questions raised by members \item
#'   \code{\link{bills}}: Bills \item \code{\link{motions}}: Motions \item
#'   \code{\link{petitions}}: Petitions \item \code{\link{addresses}}: Addresses
#'   made by members or government officials when presenting papers to the
#'   Council \item \code{\link{statements}}: Statements made by government
#'   officials \item \code{\link{voting_results}}: Results of votes in council
#'   meetings \item \code{\link{summoning_bells}}: Instances of summoning bells
#'   being rung }
#'
#' @seealso LegCo API documentation for the Hansard database:
#'   \url{https://www.legco.gov.hk/odata/english/hansard-db.html}
#'
#' @name hansard-db
#'   
NULL

#' Meeting Schedule Database
#'
#' This database contains the information of LegCo committees, namely the name
#' of all LegCo committees established since the fifth term and their
#' corresponding member lists and meeting schedule.
#'
#' @param member_id the id of a LegCo member, or a vector of ids. If
#'   \code{NULL}, returns results of all LegCo members. Defaults to \code{NULL}.
#'
#' @param term_id the id of a term, or a vector of ids. If \code{NULL}, returns
#'   results of all terms. Defaults to \code{NULL}.
#'
#' @param session_id the id of a session, or a vector of ids. If \code{NULL},
#'   returns result of all sessions. Defaults to \code{NULL}.
#'
#' @param slot_id the id of a meeting slot, or a vector of ids. If \code{NULL},
#'   returns all meetings. Defaults to \code{NULL}.
#'
#' @param meet_id the id of a meeting, or a vector of ids. If \code{NULL},
#'   returns all meetings. Useful for matching meeting with records from the
#'   Attendance Database. Defaults to \code{NULL}.
#'
#' @param committee_id the id of a committee, or a vector of ids. If
#'   \code{NULL}, returns results of all committees. Defaults to \code{NULL}.
#'
#' @param code the code of a committee, or a vector of ids. If \code{NULL},
#'   returns all committees. Defaults to \code{NULL}.
#'
#' @param type the type of meeting. If \code{"open"}, returns open meetings. If
#'   \code{"closed"}, returns closed meetings. If \code{"all"}, returns all
#'   meetings. Defaults to \code{"all"}.
#'
#' @param post the post of a member in the committee, or a vector of posts.
#'   \code{"President"}, \code{"Chairman"}, \code{"Deputy Chairman"},
#'   \code{"Member"} and \code{"Convenor"} return members of the respective
#'   post. If \code{NULL}, returns members of all posts. Defaults to
#'   \code{NULL}.
#'
#' @param date only fetch the result in which the specified date falls within.
#'   Accepts character values in \code{"YYYY-MM-DD"} format, and objects of
#'   class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or
#'   anything else that can be coerced to a date with \code{as.Date()}. Defaults
#'   to \code{NULL}.
#'
#' @param extra_param additional query parameters defined in LegCo API. Must
#'   begin with \code{"&"}.
#'
#' @param n the number of record to fetch. Defaults to \code{1000}.
#'
#' @param count logical: Whether to return only the total count of records that
#'   matches the parameter(s) instead of the result. Defaults to \code{FALSE}.
#'
#' @param verbose logical: Whether to display progress messages when fetching
#'   data? Defaults to \code{TRUE}.
#'
#' @section Functions: Functions of the Meeting Schedule Database: \itemize{
#'   \item \code{\link{term}}: LegCo terms \item \code{\link{session}}: LegCo
#'   sessions \item \code{\link{committee}}: LegCo committees \item
#'   \code{\link{membership}}: Membership of LegCo committees \item
#'   \code{\link{member}}: LegCo members \item \code{\link{member_term}}: Terms
#'   served by LegCo members \item \code{\link{meeting}}: Meetings of LegCo
#'   committees \item \code{\link{meeting_committee}}: Committees of LegCo
#'   meetings }
#'
#' @seealso LegCo API documentation for the Meeting Schedule database:
#'   \url{https://www.legco.gov.hk/odata/english/attendance-db.html}
#'
#' @name meeting_schedule-db
#'   
NULL

#' Complementary Functions
#'
#' This set of complementary functions are wrappers around the database
#' functions that facilitate the use of the LegCo API or return data from the
#' API in a more usable form. All complementary functions have a prefix of
#' \code{search_}.
#'
#' @section Functions: Complementary Functions: \itemize{ \item
#'   \code{\link{search_committee}}: Search LegCo committees \item
#'   \code{\link{search_member}}: Search LegCo members \item
#'   \code{\link{search_voting_record}}: Search Voting Record in LegCo meetings
#'   \item \code{\link{search_question}}: Search full text of question put to
#'   the government by LegCo members}
#'
#' @section Notes: The complementary functions work by calling a number of
#'   database functions and combining their output, meaning that a function call
#'   usually involves multiple API calls. Use with caution to prevent reaching
#'   the API’s rate limit too quickly.
#'
#' @name comp-fun
#'   
NULL
