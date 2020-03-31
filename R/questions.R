#' Questions by LegCo Members
#'
#' Fetch questions put to the government by LegCo members in council meetings.
#'
#' This function corresponds to the \emph{Questions} data endpoint of the
#' Hansard Database.
#'
#' @param type the type of question. \code{"oral"} returns oral questions.
#'   \code{"written"} returns written questions. \code{"all"} returns all
#'   questions. Defaults to \code{"all"}.
#'
#' @inherit hansard-db
#'
#' @examples
#' \donttest{
#' # Fetch questions being put on the government during the Council meeting on February 20, 2019
#' x <- questions(from = "2019-02-20", to = "2019-02-20")
#' }
#'
#' @export
#' 
questions <- function(rundown_id = NULL, speaker_id = NULL, type = "all",
                      lang = "en", from = '1900-01-01', to = Sys.Date(),
                      floor = FALSE, n = 1000, extra_param = NULL, count = FALSE, verbose = TRUE) {
  query <- "Questions?$select=MeetingDate,QuestionType,Subject,Speaker,SpeakerID,RundownID,HansardFileURL"
  
  filter_args <- {}
  
  if (!is.null(rundown_id)) {
    filter_args <- c(filter_args, generate_filter("RundownID", rundown_id))
  }

  if (!is.null(speaker_id)) {
    filter_args <- c(filter_args, generate_filter("SpeakerID", speaker_id))
  }
  
  type <- tolower(type)
  if (type == "oral") {
    filter_args <- c(filter_args, "QuestionType eq 'Oral'")
  } else if (type == "written") {
    filter_args <- c(filter_args, "QuestionType eq 'Written'")
  }
  
  if (is.null(rundown_id)) {
    lang <- tolower(lang)
    if (floor) {
      filter_args <- c(filter_args, "HansardType eq 'Floor'")
    } else if (lang == "en") {
      filter_args <- c(filter_args, "HansardType eq 'English'")
    } else if (lang == "zh") {
      filter_args <- c(filter_args, "HansardType eq 'Chinese'")
    }
  }
  
  from <- as.Date(from)
  to <- as.Date(to)
  filter_args <- c(filter_args, paste0("MeetingDate ge datetime\'", from, 
                                       "\' and MeetingDate le datetime\'", to, "\'"))
  
  query <- paste0(query, "&$filter=", paste(filter_args, collapse = " and "))
  
  if (!is.null(extra_param)) {
    query <- paste0(query, extra_param)
  }
  
  legco_api("hansard", query, n, count, verbose)
}

#' @rdname questions
#' @export
legco_questions <- questions
